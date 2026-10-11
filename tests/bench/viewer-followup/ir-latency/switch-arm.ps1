param([ValidateSet('B','C')][string]$Arm)
$ErrorActionPreference = 'Stop'
$armRoot = 'C:\Users\wangx\Projects\CerebroNexus-backups\2026-10-11-ir-fix-203d5793'
$files = @('R/viewer_pack.R','inst/viewer/immune_repertoire/data.R','inst/viewer/immune_repertoire/settings.R','inst/viewer/immune_repertoire/visualizations.R')
foreach ($file in $files) {
  $currentHash = (Get-FileHash -LiteralPath $file).Hash
  $knownHashes = @('B','C') | ForEach-Object { (Get-FileHash -LiteralPath (Join-Path $armRoot "$_/$file")).Hash }
  if ($currentHash -notin $knownHashes) { throw "Unexpected edit in $file; no overwrite allowed." }
}
foreach ($file in $files) { Copy-Item -LiteralPath (Join-Path $armRoot "$Arm/$file") -Destination $file }
