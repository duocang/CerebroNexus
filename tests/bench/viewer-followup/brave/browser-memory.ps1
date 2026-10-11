param([string]$OutputFile)
$samples = [System.Collections.Generic.List[object]]::new()
$until = [DateTime]::UtcNow.AddSeconds(35)
while ([DateTime]::UtcNow -lt $until) {
  $processes = @(Get-Process brave -ErrorAction SilentlyContinue)
  $samples.Add(@{at=[DateTime]::UtcNow.ToString('o');rss=($processes | Measure-Object WorkingSet64 -Sum).Sum;count=$processes.Count})
  Start-Sleep -Milliseconds 100
}
$samples | ConvertTo-Json -Compress | Set-Content -LiteralPath $OutputFile -Encoding utf8
