# Measure resolved grid viewports: direct conversion of null gtable units gives
# zero-sized rectangles in the installed patchwork/grid combination.
# This local replacement retains the skill's schema and unchanged 1.5-pt audit.
write_patchwork_panel_layout <- function(plot,manifest_path,width_in,height_in,
    panel_ids=NULL,row_groups=NULL,column_groups=NULL,exemptions=list()) {
  probe <- tempfile(fileext='.pdf')
  grDevices::pdf(probe,width=width_in,height=height_in,family='Helvetica',useDingbats=FALSE)
  on.exit({grDevices::dev.off();unlink(probe)},add=TRUE)
  g <- patchwork::patchworkGrob(plot)
  grid::grid.newpage();grid::grid.draw(g);grid::grid.force()
  rows <- g$layout[grepl('^panel-[0-9]+$',g$layout$name),]
  rows <- rows[order(rows$t,rows$l),]
  if(is.null(panel_ids)) panel_ids <- letters[seq_len(nrow(rows))]
  stopifnot(length(panel_ids)==nrow(rows))
  panels <- lapply(seq_len(nrow(rows)),function(i) {
    r <- rows[i,]
    vp <- sprintf('%s.%d-%d-%d-%d',r$name,r$t,r$l,r$b,r$r)
    grid::seekViewport(vp)
    lower <- grid::deviceLoc(grid::unit(0,'npc'),grid::unit(0,'npc'),valueOnly=TRUE)
    upper <- grid::deviceLoc(grid::unit(1,'npc'),grid::unit(1,'npc'),valueOnly=TRUE)
    grid::upViewport(0)
    list(id=panel_ids[i],bbox_pt=c(lower$x,lower$y,upper$x,upper$y)*72,
      grid_id='patchwork-grid-1',row_start=r$t-1L,row_stop=r$b,col_start=r$l-1L,col_stop=r$r)
  })
  manifest <- list(schema_version=1L,backend='r-patchwork-resolved-viewports',
    figure=list(width_pt=width_in*72,height_pt=height_in*72),panels=panels,exemptions=exemptions)
  if(!is.null(row_groups)) manifest$row_groups <- row_groups
  if(!is.null(column_groups)) manifest$column_groups <- column_groups
  jsonlite::write_json(manifest,manifest_path,pretty=TRUE,auto_unbox=TRUE,digits=15)
  invisible(manifest)
}
