param([string]$Phase)
$base='tests/bench/viewer-followup/brave/evidence/final-lifecycle'
$rss=(Get-Content ($base+'.rss')) -split ' '
@{phase=$Phase;at=[DateTime]::UtcNow.ToString('o');rRss=[double]$rss[0];rPeak=[double]$rss[1];braveTotalRss=(Get-Process brave | Measure-Object WorkingSet64 -Sum).Sum} | ConvertTo-Json -Compress | Add-Content ($base+'.memory.jsonl') -Encoding utf8
