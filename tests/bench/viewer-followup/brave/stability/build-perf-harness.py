from pathlib import Path
p=Path(__file__).resolve().parent
s=(p.parent/'remaining-server.R').read_text()
s=s.replace("tests/bench/viewer-followup/brave/remaining.js","tests/bench/viewer-followup/brave/stability/perf.js")
s=s.replace("'plotExpressionSummary','compactViolinSummary')", "'plotExpressionSummary','compactViolinSummary','viewerProjectionSubsetRows','cerebroCellViewScatterPayload','viewerProjectionFirstFrameMetadata','viewerPackTrajectoryIndex','viewerPackTrajectoryFrameResource')")
s=s.replace('  .bench_scope <<- scope', '''  .bench_scope <<- scope
  trace('load_deferred_viewer_server',where=scope,print=FALSE,
    tracer=quote(stamp('module_load','start',server_file)),
    exit=quote({
      stamp('module_load','end',server_file)
      for(name in c('trajectory_cells_reactive','cv_prepare_progressive_supplement',
        'cv_build_progressive_supplement','cv_build_trajectories','cv_canonical_metadata',
        'cv_send_progressive_supplement')) {
        if(exists(name,envir=server_scope,inherits=FALSE))
          trace(name,where=server_scope,print=FALSE,
            tracer=bquote(stamp(.(name),'start')),
            exit=bquote(stamp(.(name),'end')))
      }
    }))
''')
(p/'perf-server.R').write_text(s)
(p/'perf.js').write_text((p.parent/'remaining.js').read_text())
