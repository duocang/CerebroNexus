[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$Artifact,

    [string]$RepositoryRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path,
    [string]$BaselineRef = '892097a10d013fe9db0c35a78b4ced0dd09a3fdd',
    [string]$ReferenceRef = '6c9718d78ef5e8f895281aad95d0c6594a8026ba',
    [string]$CandidateRef = 'temp/pr5-dead-code-prune-v2',

    [ValidateSet('quick', 'publication')]
    [string]$Profile = 'quick',

    [ValidateSet('timing', 'memory')]
    [string]$Mode = 'timing',

    [ValidateRange(0, 100)]
    [int]$Rounds = 0,

    [string[]]$Pages = @(),
    [ValidateSet('first', 'repeat')]
    [string[]]$Visits = @('first', 'repeat'),
    [string]$OutputRoot = '',
    [string]$RunId = ''
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Invoke-Git {
    param(
        [Parameter(Mandatory = $true)][string[]]$Arguments,
        [Parameter(Mandatory = $true)][string]$WorkingDirectory
    )
    $output = & git -C $WorkingDirectory @Arguments 2>&1
    if ($LASTEXITCODE -ne 0) {
        throw "git $($Arguments -join ' ') failed:`n$($output -join "`n")"
    }
    @($output)
}

function Resolve-Commit {
    param([string]$Ref, [string]$Root)
    $resolved = @(Invoke-Git -WorkingDirectory $Root -Arguments @(
        'rev-parse', '--verify', "$Ref^{commit}"
    ))
    $resolved[-1].Trim()
}

function Get-OrCreateDetachedWorktree {
    param(
        [string]$Label,
        [string]$Commit,
        [string]$Root,
        [string]$WorktreeRoot
    )
    $short = $Commit.Substring(0, 8)
    $path = Join-Path $WorktreeRoot "$Label-$short"
    if (Test-Path -LiteralPath $path) {
        $actualOutput = @(Invoke-Git -WorkingDirectory $path -Arguments @(
            'rev-parse', 'HEAD'
        ))
        $actual = $actualOutput[-1].Trim()
        if ($actual -ne $Commit) {
            throw "Existing worktree $path is $actual, expected $Commit"
        }
    } else {
        New-Item -ItemType Directory -Path $WorktreeRoot -Force | Out-Null
        Invoke-Git -WorkingDirectory $Root -Arguments @(
            'worktree', 'add', '--detach', $path, $Commit
        ) | Out-Host
    }
    $dirty = @(Invoke-Git -WorkingDirectory $path -Arguments @(
        'status', '--porcelain'
    ))
    if ($dirty.Count -gt 0) {
        throw "Benchmark worktree is dirty: $path"
    }
    (Resolve-Path -LiteralPath $path).Path
}

$RepositoryRoot = (Resolve-Path -LiteralPath $RepositoryRoot).Path
$Artifact = (Resolve-Path -LiteralPath $Artifact).Path
if (-not (Test-Path -LiteralPath (Join-Path $RepositoryRoot '.git'))) {
    $inside = @(Invoke-Git -WorkingDirectory $RepositoryRoot -Arguments @(
        'rev-parse', '--is-inside-work-tree'
    ))
    if ($inside[-1].Trim() -ne 'true') {
        throw "Not a Git worktree: $RepositoryRoot"
    }
}

if (-not $OutputRoot) {
    $OutputRoot = Join-Path (Split-Path -Parent $RepositoryRoot) 'CerebroNexus-benchmarks'
}
$OutputRoot = [System.IO.Path]::GetFullPath($OutputRoot)

if ($Rounds -eq 0) {
    $Rounds = if ($Profile -eq 'publication') { 5 } else { 1 }
}
if ($Profile -eq 'publication' -and $Rounds -lt 5) {
    throw 'publication profile requires at least five rounds'
}

$refs = [ordered]@{
    before_pr0 = $BaselineRef
    pr5 = $ReferenceRef
    latest = $CandidateRef
}
$commits = [ordered]@{}
foreach ($entry in $refs.GetEnumerator()) {
    $commits[$entry.Key] = Resolve-Commit -Ref $entry.Value -Root $RepositoryRoot
}

if (-not $RunId) {
    $stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
    $shorts = ($commits.Values | ForEach-Object { $_.Substring(0, 8) }) -join '-'
    $RunId = "$stamp-$Profile-$shorts"
}
if ($RunId -notmatch '^[A-Za-z0-9][A-Za-z0-9._-]*$') {
    throw 'RunId may contain only letters, numbers, dot, underscore, and hyphen'
}

$worktreeRoot = Join-Path $OutputRoot 'worktrees'
$resultDir = Join-Path (Join-Path $OutputRoot 'results') $RunId
if (Test-Path -LiteralPath $resultDir) {
    throw "Result directory already exists: $resultDir"
}
New-Item -ItemType Directory -Path $resultDir -Force | Out-Null

$worktrees = [ordered]@{}
foreach ($entry in $commits.GetEnumerator()) {
    $worktrees[$entry.Key] = Get-OrCreateDetachedWorktree `
        -Label $entry.Key `
        -Commit $entry.Value `
        -Root $RepositoryRoot `
        -WorktreeRoot $worktreeRoot
}

$rscript = (Get-Command Rscript -ErrorAction Stop).Source
$benchmarkScript = Join-Path $RepositoryRoot 'tests\bench\benchmark_viewer_1m_pages.R'
$summaryScript = Join-Path $RepositoryRoot 'tests\bench\summarize_viewer_page_comparison.R'
if (-not (Test-Path -LiteralPath $benchmarkScript)) {
    throw "Missing benchmark script: $benchmarkScript"
}
if (-not (Test-Path -LiteralPath $summaryScript)) {
    throw "Missing summary script: $summaryScript"
}

$rawPath = Join-Path $resultDir 'raw.tsv'
$logPath = Join-Path $resultDir 'run.log'
$configPath = Join-Path $resultDir 'run-config.tsv'
$harnessShaOutput = @(Invoke-Git -WorkingDirectory $RepositoryRoot -Arguments @(
    'rev-parse', 'HEAD'
))
$harnessSha = $harnessShaOutput[-1].Trim()
$harnessDirtyOutput = @(Invoke-Git -WorkingDirectory $RepositoryRoot -Arguments @(
    'status', '--porcelain'
))
$harnessDirty = $harnessDirtyOutput.Count -gt 0

$config = @(
    "key`tvalue",
    "run_id`t$RunId",
    "profile`t$Profile",
    "benchmark_mode`t$Mode",
    "rounds`t$Rounds",
    "visits`t$($Visits -join ',')",
    "artifact`t$Artifact",
    "harness_root`t$RepositoryRoot",
    "harness_git_sha`t$harnessSha",
    "harness_git_dirty`t$harnessDirty"
)
foreach ($label in $refs.Keys) {
    $config += "$label.ref`t$($refs[$label])"
    $config += "$label.sha`t$($commits[$label])"
    $config += "$label.worktree`t$($worktrees[$label])"
}
$config | Set-Content -LiteralPath $configPath -Encoding utf8

$oldProfile = $env:VIEWER_BENCH_PROFILE
$oldMode = $env:VIEWER_BENCH_MODE
$oldPages = $env:VIEWER_PAGES_ONLY
$oldVisits = $env:VIEWER_VISITS_ONLY
try {
    $env:VIEWER_BENCH_PROFILE = $Profile
    $env:VIEWER_BENCH_MODE = $Mode
    if ($Pages.Count -gt 0) {
        $env:VIEWER_PAGES_ONLY = $Pages -join ','
    } else {
        Remove-Item Env:VIEWER_PAGES_ONLY -ErrorAction SilentlyContinue
    }
    $env:VIEWER_VISITS_ONLY = $Visits -join ','

    $benchmarkArguments = @(
        "before_pr0=$($worktrees.before_pr0)",
        $Artifact,
        $rawPath,
        [string]$Rounds,
        "pr5=$($worktrees.pr5)",
        "latest=$($worktrees.latest)"
    )
    Write-Host "Run: $RunId"
    Write-Host "Results: $resultDir"
    & $rscript $benchmarkScript @benchmarkArguments 2>&1 |
        Tee-Object -LiteralPath $logPath
    $benchmarkExit = $LASTEXITCODE
} finally {
    if ($null -eq $oldProfile) {
        Remove-Item Env:VIEWER_BENCH_PROFILE -ErrorAction SilentlyContinue
    } else {
        $env:VIEWER_BENCH_PROFILE = $oldProfile
    }
    if ($null -eq $oldMode) {
        Remove-Item Env:VIEWER_BENCH_MODE -ErrorAction SilentlyContinue
    } else {
        $env:VIEWER_BENCH_MODE = $oldMode
    }
    if ($null -eq $oldPages) {
        Remove-Item Env:VIEWER_PAGES_ONLY -ErrorAction SilentlyContinue
    } else {
        $env:VIEWER_PAGES_ONLY = $oldPages
    }
    if ($null -eq $oldVisits) {
        Remove-Item Env:VIEWER_VISITS_ONLY -ErrorAction SilentlyContinue
    } else {
        $env:VIEWER_VISITS_ONLY = $oldVisits
    }
}

if (Test-Path -LiteralPath $rawPath) {
    & $rscript $summaryScript $rawPath $resultDir
    if ($LASTEXITCODE -ne 0) {
        throw 'Benchmark completed, but summary generation failed'
    }
} else {
    throw "Benchmark did not produce raw results; inspect $logPath"
}

Write-Host "Raw: $(Join-Path $resultDir 'raw.tsv')"
Write-Host "Summary: $(Join-Path $resultDir 'page-summary.md')"
Write-Host "Comparison: $(Join-Path $resultDir 'comparison.tsv')"
if ($benchmarkExit -ne 0) {
    Write-Warning "Benchmark exited with $benchmarkExit; results were preserved and summarized."
}
exit $benchmarkExit
