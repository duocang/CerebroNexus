main <- function() {
args <- commandArgs(TRUE)
root <- normalizePath(args[[1]])
crb <- normalizePath(args[[2]])
pkgload::load_all(root,quiet=TRUE)
out <- args[[3]]
Sys.setenv(NOT_CRAN="true")
chromote::set_chrome_args(setdiff(chromote::get_chrome_args(),"--disable-gpu"))
q <- function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE))
app_dir <- tempfile("linked-profile-");dir.create(app_dir)
writeLines(c(sprintf("devtools::load_all(%s,quiet=TRUE)",q(root)),
 sprintf("source(%s)",q(file.path(root,"tests/bench/viewer-followup/profile-linked-server.R"))),
 sprintf("bench_app <- launchCerebro(mode='closed',crb_file_to_load=c(Linked=%s),percentage_cells_to_show=100)",q(crb)),
 "bench_server <- bench_app$serverFuncSource()",
 "body(bench_server) <- bquote({bench_scope <- environment(); session$onFlushed(function() bench_install(bench_scope),once=TRUE); .(body(bench_server))})",
 "bench_app$serverFuncSource <- function() bench_server",
 "bench_app"),file.path(app_dir,"app.R"))
shinytest2::local_app_support(app_dir)
app <- shinytest2::AppDriver$new(app_dir,width=1619,height=950,load_timeout=120000,timeout=120000)
on.exit(try(app$stop(),silent=TRUE))
chrome <- app$get_chromote_session()
app$wait_for_value(output="load_data_number_of_cells",timeout=120000)
app$run_js("window.__linkedProfileAck=false;Shiny.addCustomMessageHandler('bench_clock',function(x){window.__linkedProfileAck=x.active;});Shiny.addCustomMessageHandler('bench_profile_ack',x=>{window.__linkedProfileAck=x;});Shiny.setInputValue('bench_profile',false,{priority:'event'});")
Sys.sleep(.5)
app$run_js("Shiny.setInputValue('bench_profile',true,{priority:'event'});")
tryCatch(app$wait_for_js("window.__linkedProfileAck===true",timeout=15000),error=function(e){print(app$get_js("JSON.stringify(window.__linkedProfileAck)"));print(app$get_logs());stop(e)})
app$run_js(paste0("window.__linked={events:[],decodes:[],bytes:0,messages:0,start:performance.now()};",
 "window.addEventListener('cerebro:linkedviews-ready',e=>__linked.events.push({at:performance.now(),detail:e.detail}));",
 "const old=window.CBViewWire;window.CBViewWire=Object.assign({},old,{unpack:function(x){const t=performance.now();const r=old.unpack(x);__linked.decodes.push({start:t,end:performance.now(),bytes:x.byteLength,keys:Object.keys(r)});return r;}});",
 "Shiny.shinyapp.$socket.addEventListener('message',e=>{__linked.messages++;__linked.bytes+=typeof e.data==='string'?new TextEncoder().encode(e.data).length:e.data.byteLength||e.data.size||0;});",
 "document.querySelector(\"a[href='#shiny-tab-coordinated_views']\").click();"))
app$wait_for_js("window.cerebroLinkedViewsState?.ready()===true",timeout=120000)
app$run_js("Shiny.setInputValue('bench_profile',false,{priority:'event'});")
app$wait_for_js("window.__linkedProfileAck===false")
jsonlite::write_json(app$get_js("({trace:__linked,summary:cerebroLinkedViewsState.summary(),errors:Array.from(document.querySelectorAll('.shiny-output-error')).filter(x=>x.offsetParent!==null).map(x=>x.innerText)})"),out,pretty=TRUE,auto_unbox=TRUE)
app$stop();chrome$parent$close()

}
main()
