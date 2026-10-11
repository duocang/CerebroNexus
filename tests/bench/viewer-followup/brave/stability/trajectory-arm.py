import pathlib,subprocess,sys,hashlib,json
p=pathlib.Path(__file__).resolve().parent
root=p.parents[4]
path='inst/viewer/trajectory/server.R'
baseline=subprocess.check_output(['git','show','5a511f56:'+path],cwd=root)
old=b'''    cells <- cbind(
      cells,
      viewerProjectionSubsetRows(metadata, cell_index, columns)
    )'''
new=b'''    ## Integer indices already establish the exact row mapping. Assign vectors
    ## directly to retain trajectory row names without rebuilding a joined
    ## data frame and validating its million row names for every consumer.
    for (column in columns) {
      cells[[column]] <- metadata[[column]][cell_index]
    }'''
assert baseline.count(old)==1
candidate=baseline.replace(old,new)
target=root/path
assert target.read_bytes().replace(b'\r\n',b'\n') in (baseline,candidate)
data=candidate if sys.argv[1] in ('C','D') else baseline
target.write_bytes(data)
plot=root/'inst/viewer/trajectory/projection_plot.R'
original_plot=subprocess.check_output(['git','show','5a511f56:inst/viewer/trajectory/projection_plot.R'],cwd=root)
old_summary=b'''output[["trajectory_number_of_selected_cells"]] <- renderUI({
  cerebroSelectionSummary(
    trajectory_projection_selected_cells(),
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]]
  )
})'''
new_summary=b'''output[["trajectory_number_of_selected_cells"]] <- renderUI({
  selection <- trajectory_projection_selected_cells()
  cerebroSelectionSummary(
    selection,
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]],
    ## Match Gene's empty-selection path: only the validated count is needed.
    metadata = if (is.null(selection)) {
      viewerProjectionFirstFrameMetadata()
    } else {
      getMetaData()
    }
  )
})'''
assert original_plot.count(old_summary)==1
candidate_plot=original_plot.replace(old_summary,new_summary)
assert plot.read_bytes().replace(b'\r\n',b'\n') in (original_plot,candidate_plot)
plot.write_bytes(candidate_plot if sys.argv[1]=='D' else original_plot)
files=[target,plot,p/('paint-server.R' if sys.argv[2].startswith('paint/') else 'perf-server.R'),
 p/('paint.js' if sys.argv[2].startswith('paint/') else 'perf.js')]
identity={'base':'5a511f56','arm':sys.argv[2],'sha256':{str(f.relative_to(root)):hashlib.sha256(f.read_bytes()).hexdigest() for f in files}}
(p/(sys.argv[2]+'.identity.json')).write_text(json.dumps(identity,indent=2)+'\n')
