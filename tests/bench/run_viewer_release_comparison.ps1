[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$Artifact,

    [string]$BPCellsSidecar = '',
    [string]$RepositoryRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path,
    [string]$ReferenceRef = '6c9718d78ef5e8f895281aad95d0c6594a8026ba',
    [string]$CandidateRef = 'temp/pr5-dead-code-prune-v2',
    [ValidateRange(1, 100)]
    [int]$Rounds = 3,
    [string[]]$Pages = @(),
    [string]$OutputRoot = '',
    [string]$RunId = ''
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Invoke-Git {
    param([string]$Root, [string[]]$Arguments)
    $output = @(& git -C $Root @Arguments 2>&1)
    if ($LASTEXITCODE -ne 0) {
        throw "git $($Arguments -join ' ') failed:`n$($output -join "`n")"
    }
    $output
}

function Resolve-Commit {
    param([string]$Root, [string]$Ref)
    $output = @(Invoke-Git $Root @('rev-parse', '--verify', "$Ref^{commit}"))
    $output[-1].Trim()
}

function Get-BenchmarkWorktree {
    param([string]$Root, [string]$Label, [string]$Commit, [string]$Parent)
    $path = Join-Path $Parent "$Label-$($Commit.Substring(0, 8))"
    if (-not (Test-Path -LiteralPath $path)) {
        New-Item -ItemType Directory -Path $Parent -Force | Out-Null
        Invoke-Git $Root @('worktree', 'add', '--detach', $path, $Commit) |
            Out-Host
    }
    $actual = @(Invoke-Git $path @('rev-parse', 'HEAD'))[-1].Trim()
    if ($actual -ne $Commit) {
        throw "Benchmark worktree $path is $actual, expected $Commit"
    }
    $dirty = @(Invoke-Git $path @('status', '--porcelain'))
    if ($dirty.Count -gt 0) {
        throw "Benchmark worktree is dirty: $path"
    }
    (Resolve-Path -LiteralPath $path).Path
}

function New-CandidateFixture {
    param(
        [string]$Label,
        [string]$SourceArtifact,
        [string]$SourceSidecar,
        [string]$FixtureRoot,
        [string]$CandidateRoot,
        [string]$Rscript
    )
    $directory = Join-Path $FixtureRoot $Label
    New-Item -ItemType Directory -Path $directory -Force | Out-Null
    $artifact = Join-Path $directory (Split-Path -Leaf $SourceArtifact)
    if (-not (Test-Path -LiteralPath $artifact)) {
        New-Item -ItemType HardLink -Path $artifact -Target $SourceArtifact |
            Out-Null
    }
    $sidecar = Join-Path $directory (Split-Path -Leaf $SourceSidecar)
    if (-not (Test-Path -LiteralPath $sidecar)) {
        New-Item -ItemType Junction -Path $sidecar -Target $SourceSidecar |
            Out-Null
    }
    $rootR = $CandidateRoot.Replace('\', '/')
    $artifactR = $artifact.Replace('\', '/')
    $expression = "pkgload::load_all('$rootR', quiet=TRUE);" +
        "buildViewerPack('$artifactR', viewer_binary='always', overwrite=TRUE)"
    & $Rscript -e $expression
    if ($LASTEXITCODE -ne 0) {
        throw "Viewer Pack build failed for $Label"
    }
    $artifact
}

$RepositoryRoot = (Resolve-Path -LiteralPath $RepositoryRoot).Path
$Artifact = (Resolve-Path -LiteralPath $Artifact).Path
if (-not $BPCellsSidecar) {
    $sidecars = @(Get-ChildItem -LiteralPath (Split-Path -Parent $Artifact) `
        -Directory -Filter '*.bpcells')
    if ($sidecars.Count -ne 1) {
        throw 'Specify -BPCellsSidecar when the artifact directory does not contain exactly one *.bpcells directory.'
    }
    $BPCellsSidecar = $sidecars[0].FullName
}
$BPCellsSidecar = (Resolve-Path -LiteralPath $BPCellsSidecar).Path

if (-not $OutputRoot) {
    $OutputRoot = Join-Path (Split-Path -Parent $RepositoryRoot) `
        'CerebroNexus-benchmarks'
}
$OutputRoot = [System.IO.Path]::GetFullPath($OutputRoot)
$rscript = (Get-Command Rscript -ErrorAction Stop).Source

$refs = [ordered]@{ pr5 = $ReferenceRef; latest = $CandidateRef }
$commits = [ordered]@{}
foreach ($entry in $refs.GetEnumerator()) {
    $commits[$entry.Key] = Resolve-Commit $RepositoryRoot $entry.Value
}
if (-not $RunId) {
    $RunId = "$(Get-Date -Format 'yyyyMMdd-HHmmss')-3x-first-pr5-latest"
}
if ($RunId -notmatch '^[A-Za-z0-9][A-Za-z0-9._-]*$') {
    throw 'RunId may contain only letters, numbers, dot, underscore, and hyphen'
}

$resultDir = Join-Path (Join-Path $OutputRoot 'results') $RunId
if (Test-Path -LiteralPath $resultDir) {
    throw "Result directory already exists: $resultDir"
}
New-Item -ItemType Directory -Path $resultDir -Force | Out-Null
$worktreeRoot = Join-Path $OutputRoot 'worktrees'
$fixtureRoot = Join-Path (Join-Path $OutputRoot 'fixtures') $RunId

$worktrees = [ordered]@{}
$fixtures = [ordered]@{}
foreach ($label in $refs.Keys) {
    $worktrees[$label] = Get-BenchmarkWorktree `
        -Root $RepositoryRoot -Label $label -Commit $commits[$label] `
        -Parent $worktreeRoot
    $fixtures[$label] = New-CandidateFixture `
        -Label $label -SourceArtifact $Artifact `
        -SourceSidecar $BPCellsSidecar -FixtureRoot $fixtureRoot `
        -CandidateRoot $worktrees[$label] -Rscript $rscript
}

$config = @("key`tvalue", "run_id`t$RunId", "rounds`t$Rounds")
foreach ($label in $refs.Keys) {
    $config += "$label.ref`t$($refs[$label])"
    $config += "$label.sha`t$($commits[$label])"
    $config += "$label.worktree`t$($worktrees[$label])"
    $config += "$label.artifact`t$($fixtures[$label])"
}
$config | Set-Content -LiteralPath (Join-Path $resultDir 'run-config.tsv') `
    -Encoding utf8

$benchmark = Join-Path $RepositoryRoot 'tests\bench\benchmark_viewer_1m_pages.R'
$summarizer = Join-Path $RepositoryRoot 'tests\bench\summarize_viewer_page_comparison.R'
$oldProfile = $env:VIEWER_BENCH_PROFILE
$oldPages = $env:VIEWER_PAGES_ONLY
$oldVisits = $env:VIEWER_VISITS_ONLY
$exitCodes = @()
try {
    $env:VIEWER_BENCH_PROFILE = 'quick'
    $env:VIEWER_VISITS_ONLY = 'first'
    if ($Pages.Count) {
        $env:VIEWER_PAGES_ONLY = $Pages -join ','
    } else {
        Remove-Item Env:VIEWER_PAGES_ONLY -ErrorAction SilentlyContinue
    }
    foreach ($label in $refs.Keys) {
        $raw = Join-Path $resultDir "$label.tsv"
        $log = Join-Path $resultDir "$label.log"
        & $rscript $benchmark `
            "$label=$($worktrees[$label])" $fixtures[$label] $raw $Rounds `
            2>&1 | Tee-Object -LiteralPath $log
        $exitCodes += $LASTEXITCODE
    }
} finally {
    if ($null -eq $oldProfile) { Remove-Item Env:VIEWER_BENCH_PROFILE -ErrorAction SilentlyContinue } else { $env:VIEWER_BENCH_PROFILE = $oldProfile }
    if ($null -eq $oldPages) { Remove-Item Env:VIEWER_PAGES_ONLY -ErrorAction SilentlyContinue } else { $env:VIEWER_PAGES_ONLY = $oldPages }
    if ($null -eq $oldVisits) { Remove-Item Env:VIEWER_VISITS_ONLY -ErrorAction SilentlyContinue } else { $env:VIEWER_VISITS_ONLY = $oldVisits }
}

$pr5R = (Join-Path $resultDir 'pr5.tsv').Replace('\', '/')
$latestR = (Join-Path $resultDir 'latest.tsv').Replace('\', '/')
$combined = Join-Path $resultDir 'raw.tsv'
$combinedR = $combined.Replace('\', '/')
& $rscript -e "x<-rbind(read.delim('$pr5R'),read.delim('$latestR'));write.table(x,'$combinedR',sep='\t',row.names=FALSE,quote=FALSE,na='NA')"
if ($LASTEXITCODE -ne 0) { throw 'Could not combine candidate results' }
& $rscript $summarizer $combined $resultDir
if ($LASTEXITCODE -ne 0) { throw 'Could not summarize candidate results' }

Write-Host "Results: $resultDir"
if ($exitCodes | Where-Object { $_ -ne 0 }) {
    Write-Warning 'At least one strict performance gate failed; all measurements were preserved.'
}
