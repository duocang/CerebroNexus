##----------------------------------------------------------------------------##
## The Spatial page shell, including its cell-view host, is declared statically
## in UI.R. Keeping the host in the initial document lets the tab-click handler
## request the million-cell frame immediately instead of waiting for a renderUI
## round trip. Page-specific dynamic controls remain server outputs mounted into
## that shell.
##----------------------------------------------------------------------------##
