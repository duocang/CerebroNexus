# Read installed Shiny defaults; no session or server configuration mutation.
cat('Shiny ',as.character(packageVersion('shiny')),'\n',sep='')
generator <- shiny:::ShinySession
for(section in c('public_fields','private_fields','public_methods','private_methods')) {
  values <- generator[[section]]
  for(name in grep('reconnect|resume',names(values),ignore.case=TRUE,value=TRUE)) {
    cat(section,' ',name,'\n',sep='');print(values[[name]])
  }
}
javascript <- system.file('www','shared','shiny.js',package='shiny')
lines <- readLines(javascript,warn=FALSE)
indices <- grep('allowReconnect',lines,fixed=TRUE)
for(i in indices)cat(i,': ',lines[[i]],'\n',sep='')
