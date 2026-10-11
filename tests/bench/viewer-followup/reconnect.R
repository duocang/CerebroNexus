start_rss_monitor <- function(r_pid, chrome_pid) {
  result <- tempfile("viewer-page-rss-", fileext = ".rds")
  stop_file <- paste0(result, ".stop")
  ready_file <- paste0(result, ".ready")
  process <- callr::r_bg(
    function(r_pid, chrome_pid, result, stop_file, ready_file) {
      tree_rss <- function(root) {
        handle <- tryCatch(ps::ps_handle(root), error = function(error) NULL)
        if (is.null(handle) || !ps::ps_is_running(handle)) {
          return(NA_real_)
        }
        children <- tryCatch(
          ps::ps_children(handle, recursive = TRUE),
          error = function(error) list()
        )
        rss <- vapply(
          c(list(handle), children),
          function(member) {
            tryCatch(
              unname(ps::ps_memory_info(member)[["rss"]]),
              error = function(error) NA_real_
            )
          },
          numeric(1L)
        )
        rss <- rss[is.finite(rss)]
        if (!length(rss)) {
          return(NA_real_)
        }
        sum(rss) / 1024
      }
      update_peak <- function(current, observed) {
        if (!is.finite(observed)) {
          return(current)
        }
        if (!is.finite(current)) observed else max(current, observed)
      }
      peaks <- c(r_peak_rss_kib = NA_real_, chrome_peak_rss_kib = NA_real_)
      file.create(ready_file)
      repeat {
        peaks[[1L]] <- update_peak(peaks[[1L]], tree_rss(r_pid))
        peaks[[2L]] <- update_peak(peaks[[2L]], tree_rss(chrome_pid))
        if (file.exists(stop_file)) {
          break
        }
        Sys.sleep(0.05)
      }
      saveRDS(peaks, result)
    },
    args = list(r_pid, chrome_pid, result, stop_file, ready_file),
    stdout = "|",
    stderr = "|"
  )
  deadline <- Sys.time() + 5
  while (
    !file.exists(ready_file) && process$is_alive() && Sys.time() < deadline
  ) {
    Sys.sleep(0.01)
  }
  if (!file.exists(ready_file)) {
    process$kill()
    stop("RSS monitor did not start.", call. = FALSE)
  }
  list(process = process, result = result, stop = stop_file, ready = ready_file)
}

stop_rss_monitor <- function(monitor) {
  file.create(monitor$stop)
  # A recursive Windows process-tree sample can take several seconds. Give an
  # in-flight sample time to finish and persist its peaks after the stop signal.
  monitor$process$wait(30000)
  value <- if (file.exists(monitor$result)) {
    readRDS(monitor$result)
  } else {
    c(r_peak_rss_kib = NA_real_, chrome_peak_rss_kib = NA_real_)
  }
  unlink(c(monitor$result, monitor$stop, monitor$ready))
  value
}


main <- function() {
args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
chromote::set_chrome_args(setdiff(chromote::get_chrome_args(),"--disable-gpu"))
if (length(args) != 4L) {
  stop("usage: Rscript check-session.R ROOT CRB_A CRB_B OUTPUT_TSV")
}
root <- normalizePath(args[[1L]])
paths <- normalizePath(args[2:3])
output <- normalizePath(args[[4L]], mustWork = FALSE)
pkgload::load_all(root, quiet = TRUE)
counts <- vapply(paths, function(path) nrow(readCerebro(path)$getMetaData()), integer(1))
names(counts) <- c("A", "B")
genes <- lapply(paths,function(path)head(readCerebro(path)$getGeneNames(),2L))
genes[[1]] <- c("Gad1","Slc17a7"); names(genes) <- c("A","B")

app_dir <- tempfile("viewer-lifecycle-")
dir.create(app_dir)
on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
quote_r <- function(value) encodeString(value, quote = '"')
writeLines(c(
  sprintf("devtools::load_all(%s, quiet=TRUE)", quote_r(root)),
  sprintf("launchCerebro(mode='closed', crb_file_to_load=c(A=%s,B=%s),percentage_cells_to_show=100,projections_show_hover_info=TRUE)",
    quote_r(paths[[1L]]), quote_r(paths[[2L]]))
), file.path(app_dir, "app.R"))
shinytest2::local_app_support(app_dir)
app <- shinytest2::AppDriver$new(app_dir, name = "viewer_session_v3",
  height = 950, width = 1619, load_timeout = 120000)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
on.exit(try(writeLines(capture.output(app$get_logs()),paste0(output,".logs.txt")),silent=TRUE),add=TRUE,after=FALSE)
app$wait_for_js("!!document.getElementById('crb_file_selector')?.selectize && !!window.CerebroDatasetContext",
  timeout = 120000)
app$run_js(paste0(
  "window.__lifecycleAck=null;",
  "Shiny.addCustomMessageHandler('cerebro_benchmark_dataset_ready',",
  "function(x){window.__lifecycleAck=x;});"
))
app$run_js(paste0("window.__sessionDraws=[];",
  "window.addEventListener('cerebro:specialist-state',e=>{",
  "if(e.detail.viewId==='expression_projection')__sessionDraws.push(e.detail);});"))
app$run_js(paste0("window.__contextTrace=[];",
  "Shiny.addCustomMessageHandler('cerebro_dataset_context',x=>{",
  "const accepted=CerebroDatasetContext.receive(x);",
  "__contextTrace.push({message:x,accepted:accepted,state:CerebroDatasetContext.snapshot()});});",
  "const oldInput=Shiny.setInputValue;Shiny.setInputValue=function(n,v,...rest){",
  "if(n==='viewer_dataset_selection'||n==='viewer_dataset_context_sync')__contextTrace.push({input:n,value:v});",
  "return oldInput.call(this,n,v,...rest);};"))

rss_mib <- function(pid) {
  handle <- tryCatch(ps::ps_handle(pid), error = function(error) NULL)
  if (is.null(handle)) return(NA_real_)
  children <- tryCatch(ps::ps_children(handle, recursive = TRUE),
    error = function(error) list())
  sum(vapply(c(list(handle), children), function(x) {
    tryCatch(ps::ps_memory_info(x)[["rss"]],
      error = function(error) 0)
  }, numeric(1))) / 2^20
}
r_pid <- app$.__enclos_env__$private$shiny_process$get_pid()
browser <- app$get_chromote_session()$parent
on.exit(try(browser$close(), silent = TRUE), add = TRUE)
browser_pid <- browser$get_browser()$.__enclos_env__$private$process$get_pid()
monitor <- start_rss_monitor(r_pid,browser_pid)
on.exit({peak <- stop_rss_monitor(monitor);jsonlite::write_json(as.list(peak),paste0(output,".rss.json"),auto_unbox=TRUE,pretty=TRUE)},add=TRUE,after=FALSE)


rows <- list()
cache_records <- list()
record <- function(step) {
 ctx <- app$get_js("CerebroDatasetContext.current()")
 cache_records[[length(cache_records)+1L]] <<- list(step=step,context=ctx,linked=app$get_js("window.cerebroLinkedViewsState?.summary()||null"))
 jsonlite::write_json(cache_records,paste0(output,".cache.json"),auto_unbox=TRUE,pretty=TRUE)
 rows[[length(rows)+1L]] <<- data.frame(step=step,selection=ctx$dataset_key,generation=ctx$generation,r_after_mib=rss_mib(r_pid),browser_after_mib=rss_mib(browser_pid))
 write.table(do.call(rbind,rows),output,sep="\t",row.names=FALSE,quote=TRUE)
}
record("start")
after_reload <- FALSE
visit_gene <- function(label, gene) {
 was_active <- app$get_js("document.querySelector('.sidebar-menu li.active a')?.getAttribute('href')==='#shiny-tab-geneExpression'")
 app$run_js(paste0("__sessionDraws=[];document.querySelector(\"a[href='#shiny-tab-geneExpression']\").click();"))
 app$wait_for_js("!!document.getElementById('expression_genes_input')?.selectize",timeout=120000)
 # Lifecycle setup only: wait for the existing page to paint, then allow its
 # selector restoration to settle before starting a new user operation.
 # Idle is not used as a page-ready event or a performance measurement.
 if(!isTRUE(was_active)) app$wait_for_js(sprintf("__sessionDraws.some(x=>['primary','cached'].includes(x.eventKind)&&x.renderedPointCount===%s&&x.dataset_context.generation===CerebroDatasetContext.current().generation)",counts[[label]]),timeout=120000)
 if(!after_reload) app$wait_for_idle(duration=1000,timeout=120000)

 app$run_js(sprintf("(()=>{const s=document.getElementById('expression_genes_input').selectize;s.addOption({value:%s,text:%s});s.setValue(%s);})()",quote_r(gene),quote_r(gene),quote_r(gene)))
 app$wait_for_js(sprintf("__sessionDraws.some(x=>['primary','cached'].includes(x.eventKind)&&x.renderKey?.includes(%s)&&x.renderedPointCount===%s&&x.dataset_context.generation===CerebroDatasetContext.current().generation)",quote_r(gene),counts[[label]]),timeout=120000)
 app$wait_for_js("document.querySelector('#expression_projection_cell_view_host')?.innerText.includes(" %+% quote_r(gene) %+% ")",timeout=30000)
 record(paste("gene",label,gene))
}
`%+%` <- paste0
visit_linked <- function(label) {
 app$run_js("document.querySelector(\"a[href='#shiny-tab-coordinated_views']\").click();")
 app$wait_for_js("cerebroLinkedViewsState.ready()",timeout=120000)
 ctx <- app$get_js("CerebroDatasetContext.current()")
 state <- app$get_js("cerebroLinkedViewsState.summary()")
 stopifnot(identical(state$datasetFingerprint, app$get_js("__lifecycleAck.dataset_fingerprint")),state$selectedCells==0L)
 record(paste("linked",label))
}

load_selected <- function(label, step) {
  app$wait_for_js(sprintf("CerebroDatasetContext.current()?.dataset_key===%s&&document.getElementById('crb_file_selector').value===%s",quote_r(label),quote_r(label)),timeout=30000)
  token <- paste0("lifecycle-", step, "-", label)
  app$run_js(sprintf(paste0(
    "window.__lifecycleAck=null;",
    "Shiny.setInputValue('cerebro_benchmark_dataset_load_request',%s,",
    "{priority:'event'});"
  ), quote_r(token)))
  app$wait_for_js(sprintf(paste0(
    "window.__lifecycleAck?.request===%s&&",
    "window.__lifecycleAck?.cell_count===%s&&",
    "window.CerebroDatasetContext.phase()==='ready'"
  ), quote_r(token), counts[[label]]), timeout = 120000)
  state <- app$get_js(paste0(
    "(() => {const C=window.CerebroDatasetContext;",
    "return {epoch:C.current()?.epoch,key:C.current()?.dataset_key,",
    "generation:C.current()?.generation,",
    "phase:C.phase(),selected:document.getElementById('crb_file_selector').value,",
    "fingerprint:window.__lifecycleAck?.dataset_fingerprint};})()"
  ))
  stopifnot(identical(state$key, label), identical(state$selected, label),
    identical(state$phase, "ready"),
    grepl("^md5-cell-set-v1:", state$fingerprint))
  record(paste("dataset",step,label))
  cat("dataset switch/render", step, label, "PASS\n")
  invisible(state)
}

app$run_js("document.getElementById('crb_file_selector').selectize.setValue('A');")
initial <- load_selected("A", 0L)
visit_gene("A",genes$A[[1]])
record("before_disconnect")
before_disconnect <- app$get_js("window.CerebroDatasetContext.current()")
jsonlite::write_json(app$get_js("({allowReconnect:Shiny.shinyapp.$allowReconnect,context:CerebroDatasetContext.current()})"),paste0(output,".transport.json"),auto_unbox=TRUE,pretty=TRUE)
app$run_js("Shiny.shinyapp.$socket.close();")
app$wait_for_js("Shiny.shinyapp.$socket===null||Shiny.shinyapp.$socket.readyState===3",
  timeout = 30000)
# Standalone Shiny explicitly disables resumable reconnect. Test supported
# recovery by reloading the document, without calling private reconnect().
app$get_chromote_session()$Page$reload()
app$wait_for_js(paste0("!!document.getElementById('crb_file_selector')?.selectize&&",
  "!!window.CerebroDatasetContext&&Shiny.shinyapp.$socket?.readyState===1"), timeout=120000)
app$run_js(paste0("window.__lifecycleAck=null;window.__sessionDraws=[];",
  "Shiny.addCustomMessageHandler('cerebro_benchmark_dataset_ready',x=>{__lifecycleAck=x;});",
  "window.addEventListener('cerebro:specialist-state',e=>{",
  "if(e.detail.viewId==='expression_projection')__sessionDraws.push(e.detail);});"))
app$run_js("document.getElementById('crb_file_selector').selectize.setValue('A');")
reloaded <- load_selected("A", 9L)
after_reload <- TRUE
visit_gene("A",genes$A[[1]])
record("reloaded")
stopifnot(!identical(reloaded$epoch, before_disconnect$epoch))
old_context_json <- jsonlite::toJSON(before_disconnect, auto_unbox=TRUE)
stopifnot(!app$get_js(sprintf(
  "CerebroDatasetContext.accepts({dataset_context:%s})", old_context_json)))
out <- do.call(rbind, rows)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.table(out, output, sep = "\t", row.names = FALSE, quote = FALSE)
print(out)
cat("disconnect/reload, exact Gene label and 1M points, stale epoch rejection: PASS\n")
}
main()
