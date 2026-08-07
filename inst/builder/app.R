##----------------------------------------------------------------------------##
## Build Cerebro data sets from Seurat objects, by pointing and clicking.
##
## Several objects per session: each becomes a .crb, and they are bundled into
## one app with the data set switcher, which is how a lab usually wants to hand
## a project over -- one link, every sample behind a dropdown.
##
## Objects are read into this process, so this is a tool for your own machine
## or your own RStudio Server session, not something to deploy for others.
##----------------------------------------------------------------------------##

library(shiny)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Above this many levels the interface stops offering one swatch each: nobody
## edits 200 colours by hand, and 200 colour inputs make the page unusable.
BUILDER_SWATCH_MAX <- 40L

## runApp() sets the working directory to the app directory.
source("io.R", local = TRUE)
source(
  file.path(
    "..",
    "shiny",
    "v1.4",
    "core",
    "viewer_content_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "shiny",
    "v1.4",
    "core",
    "spatial_coordinate_contract.R"
  ),
  local = TRUE
)
source("spatial.R", local = TRUE)
source(
  file.path(
    "..",
    "shiny",
    "v1.4",
    "hla_tcr_motifs",
    "core",
    "hla_typing.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "shiny",
    "v1.4",
    "hla_tcr_motifs",
    "core",
    "hla_motif_core.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "shiny",
    "v1.4",
    "hla_tcr_motifs",
    "core",
    "hla_association_core.R"
  ),
  local = TRUE
)
source("manifest.R", local = TRUE)
source("content_tables.R", local = TRUE)
source("content_immune.R", local = TRUE)
source("content_spatial.R", local = TRUE)
source("content.R", local = TRUE)
source("profile.R", local = TRUE)
source("inspect.R", local = TRUE)
source("adapters.R", local = TRUE)
source("preview.R", local = TRUE)
source("extras.R", local = TRUE)
source("analysis.R", local = TRUE)
source("prerequisite.R", local = TRUE)
source("state.R", local = TRUE)
source("plan.R", local = TRUE)
source("session.R", local = TRUE)

app_capability <- builder_app_capability()

## Inline icons: an icon set would be another dependency, and emoji are not
## icons.
icon_svg <- function(path, label = NULL) {
  tags$svg(
    class = "icon",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = if (is.null(label)) "true" else NULL,
    role = if (is.null(label)) NULL else "img",
    if (!is.null(label)) tags$title(label),
    tags$path(d = path)
  )
}
ICON_PLUS <- "M12 5v14M5 12h14"
ICON_TRASH <- "M3 6h18M8 6V4h8v2M19 6l-1 14H6L5 6"
ICON_FOLDER <- "M3 7a2 2 0 012-2h4l2 2h8a2 2 0 012 2v8a2 2 0 01-2 2H5a2 2 0 01-2-2z"

## The viewer's wordmark, inlined. The builder and the viewer are one product
## and should look like it; inlining avoids a resource path that would have to
## resolve both from the installed package and from a source checkout.
cerebro_wordmark <- local({
  candidates <- c(
    system.file(
      "shiny/v1.4/www/cerebronexus.svg",
      package = "CerebroNexus"
    ),
    file.path("..", "shiny", "v1.4", "www", "cerebronexus.svg")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) {
    HTML(paste(readLines(hit[1], warn = FALSE), collapse = "\n"))
  } else {
    NULL
  }
})

## Stamp the assets so a browser that already has them does not keep them past
## an upgrade. Shiny serves www/ with caching on, and a cached stylesheet
## against new markup is worse than either alone -- the classes change and the
## rules that give them meaning do not arrive. The stamp is the file's own
## mtime, so it changes exactly when the file does, including while editing.
asset_stamp <- function(file) {
  mt <- tryCatch(file.mtime(file), error = function(e) NA)
  if (is.na(mt)) {
    return("")
  }
  paste0("?v=", as.integer(as.numeric(mt)))
}

ui <- tagList(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = paste0("builder.css", asset_stamp("www/builder.css"))
    ),
    tags$script(src = paste0("builder.js", asset_stamp("www/builder.js"))),
    tags$title("Cerebro Dataset Builder")
  ),
  div(
    class = "topbar",
    div(class = "wordmark", cerebro_wordmark),
    div(class = "divider"),
    h1("Dataset Builder"),
    span(
      class = "hint",
      style = "font-size:.82rem;margin:0",
      "Turn Seurat objects into a ready-to-run visual app"
    ),
    uiOutput("busy", inline = TRUE),
    span(class = "formats", textOutput("format_line", inline = TRUE))
  ),
  div(
    class = "shell",
    div(
      class = "rail",
      div(
        class = "rail-head",
        span("Datasets"),
        textOutput("ds_count", inline = TRUE)
      ),
      uiOutput("ds_list"),
      div(
        class = "rail-add",
        actionButton(
          "open_browser",
          tagList(icon_svg(ICON_FOLDER), " Choose files…"),
          class = "btn btn-primary",
          style = "width:100%"
        ),
        div(class = "or", "or explore an example"),
        uiOutput("example_buttons"),
        uiOutput("add_error")
      )
    ),
    div(id = "pane", uiOutput("result_card"), uiOutput("detail"))
  ),
  uiOutput("browser_panel"),
  uiOutput("actionbar")
)

server <- function(input, output, session) {
  ## Each entry: id, path, format, object, profile, settings.
  ## `settings` is what the user chose; it is written back whenever an input
  ## changes so switching between data sets does not lose it.
  sets <- reactiveVal(list())
  current <- reactiveVal(NULL)
  result <- reactiveVal(NULL)
  seq_id <- reactiveVal(0L)
  add_error <- reactiveVal(NULL)
  preview_frame <- reactiveVal(NULL)
  spatial_coords <- reactiveVal(NULL)

  ## The id of the data set that arrived on its own, rather than because the
  ## user pointed at it -- it gets one extra beat of animation so a load that
  ## finished while the user was reading is not silent.
  just_added <- reactiveVal(NULL)

  entry_of <- function(id) {
    all <- sets()
    hit <- Filter(function(e) identical(e$id, id), all)
    if (length(hit)) hit[[1]] else NULL
  }

  ## Names end up as the app's dataset switcher labels, so duplicates are not
  ## cosmetic: they block the build. Loading the same example twice is an
  ## obvious thing to do, so suffix rather than refuse.
  unique_name <- function(label) {
    taken <- vapply(sets(), function(e) e$settings$name, character(1))
    if (!label %in% taken) {
      return(label)
    }
    n <- 2L
    while (paste0(label, " ", n) %in% taken) {
      n <- n + 1L
    }
    paste0(label, " ", n)
  }

  replace_entry <- function(updated) {
    all <- sets()
    for (i in seq_along(all)) {
      if (identical(all[[i]]$id, updated$id)) all[[i]] <- updated
    }
    sets(all)
  }

  output$format_line <- renderText(builder_format_summary())
  output$ds_count <- renderText({
    n <- length(sets())
    if (n == 0) "" else paste0(n)
  })

  ## -- the worker process ---------------------------------------------------
  ## Objects live there, never here. `pending` names the call we are waiting
  ## for; the poller below turns its answer into state.
  worker <- reactiveVal(NULL)
  pending <- reactiveVal(NULL)
  ## A queue rather than a single slot: requests arrive while the worker is
  ## busy (picking a data set fires both a preview and a coordinate fetch),
  ## and dropping the second one silently is how a panel ends up permanently
  ## empty.
  queue <- reactiveVal(list())
  request_sequence <- reactiveVal(0L)
  latest_request <- reactiveVal(list())
  enqueue <- function(req) {
    q <- queue()
    ## One outstanding request per kind: the newest slider position is the
    ## only one worth answering.
    if (!is.null(req$replaces)) {
      q <- Filter(function(x) !identical(x$kind, req$replaces), q)
      request_sequence(request_sequence() + 1L)
      key <- req$replaces
      latest <- latest_request()
      latest[[key]] <- request_sequence()
      latest_request(latest)
      req$request_key <- key
      req$request_generation <- request_sequence()
    }
    queue(c(q, list(req)))
  }
  busy_note <- reactiveVal(NULL)

  observe({
    if (!is.null(worker())) {
      return()
    }
    started <- builder_session_start(getwd())
    if (!is.null(started$error)) {
      add_error(started$error)
      return()
    }
    worker(started$session)
  })

  session$onSessionEnded(function() {
    rs <- isolate(worker())
    if (!is.null(rs)) {
      try(rs$close(), silent = TRUE)
    }
  })

  ## -- file browser and examples --------------------------------------------
  ## A browser of our own rather than shinyFiles: its dialog is a Bootstrap
  ## modal, and this page deliberately does not load Bootstrap, so the dialog
  ## arrives unstyled. Ours is ~40 lines and matches everything else.
  ##
  ## Whether the sheet is open and which directory it shows are two separate
  ## pieces of state on purpose. Folding them into one -- a directory that is
  ## NULL when closed -- meant the whole overlay was one render keyed on the
  ## directory, so every folder click rebuilt the backdrop and replayed its
  ## entrance fade: measured, the overlay dropped to opacity 0 and climbed back
  ## over ~130 ms on each navigation. Split this way, navigating replaces only
  ## the listing and the backdrop node survives untouched.
  browse_open <- reactiveVal(FALSE)
  browse_dir <- reactiveVal(path.expand("~"))

  observeEvent(input$open_browser, browse_open(TRUE))
  observeEvent(input$close_browser, browse_open(FALSE))
  observeEvent(input$browse_to, browse_dir(input$browse_to))

  observeEvent(input$choose_file, {
    path <- input$choose_file
    browse_open(FALSE)
    if (any(vapply(sets(), function(e) identical(e$path, path), logical(1)))) {
      add_error("This file has already been added.")
      return()
    }
    start_load("file", path, tools::file_path_sans_ext(basename(path)))
  })

  ## The chrome: rendered when the sheet opens and then left alone. It reads
  ## `browse_open()` and deliberately never reads `browse_dir()`, which is what
  ## keeps navigation from touching it.
  output$browser_panel <- renderUI({
    if (!browse_open()) {
      return(NULL)
    }
    roots <- builder_browse_roots()
    div(
      class = "sheet-backdrop",
      onclick = "if (event.target === this) Shiny.setInputValue('close_browser', Math.random())",
      div(
        class = "sheet",
        div(
          class = "sheet-head",
          span(class = "sheet-title", "Choose a Seurat object"),
          tags$button(
            class = "btn btn-quiet",
            onclick = "Shiny.setInputValue('close_browser', Math.random())",
            "Close"
          )
        ),
        div(
          class = "sheet-roots",
          lapply(names(roots), function(nm) {
            tags$button(
              class = "chip-btn builder-browse",
              `data-path` = unname(roots[[nm]]),
              nm
            )
          })
        ),
        uiOutput("browse_list", class = "sheet-body")
      )
    )
  })

  ## The part that actually changes when you navigate.
  output$browse_list <- renderUI({
    dir <- browse_dir()
    listing <- builder_browse(dir)
    tagList(
      div(class = "sheet-path", listing$dir %||% dir),
      if (!is.null(listing$error)) {
        div(class = "notice bad", listing$error)
      } else {
        div(
          class = "sheet-list",
          if (!identical(listing$dir, listing$parent)) {
            tags$button(
              class = "row-btn builder-browse",
              `data-path` = listing$parent,
              span(class = "row-icon", "↑"),
              span(class = "row-name", "Parent folder")
            )
          },
          lapply(listing$dirs, function(d) {
            tags$button(
              class = "row-btn builder-browse",
              `data-path` = file.path(listing$dir, d),
              span(class = "row-icon", "▸"),
              span(class = "row-name", d)
            )
          }),
          if (nrow(listing$files)) {
            lapply(seq_len(nrow(listing$files)), function(i) {
              tags$button(
                class = "row-btn is-file builder-choose",
                `data-path` = listing$files$path[i],
                span(class = "row-icon", "•"),
                span(class = "row-name", listing$files$name[i]),
                span(
                  class = "row-size",
                  sprintf("%.1f MB", listing$files$size[i] / 1048576)
                )
              )
            })
          } else {
            div(
              class = "hint",
              style = "padding:.8rem",
              "No supported object files were found in this folder."
            )
          }
        )
      }
    )
  })
  ## Always computed, so the listing is ready the instant the sheet opens
  ## rather than arriving a frame later into an empty panel.
  outputOptions(output, "browse_list", suspendWhenHidden = FALSE)

  ## Rendered ONCE -- this reads no reactive value, so the buttons are created
  ## with the session and never replaced. That is what makes the leave
  ## animation possible: a renderUI that re-ran on every change would destroy
  ## the node before it could animate out, and only entrances would ever be
  ## seen. Which examples are in use is a class the client toggles instead.
  output$example_buttons <- renderUI({
    tagList(lapply(builder_examples(), function(ex) {
      tags$button(
        class = "btn example-btn",
        `data-ex` = ex$id,
        ## One wrapper, because the collapse animates the button's single grid
        ## row to 0fr -- two children would be two rows and only the first
        ## would close.
        tags$span(
          class = "ex-inner",
          tags$span(class = "ex-label", ex$label),
          tags$span(class = "ex-detail", ex$detail)
        )
      )
    }))
  })

  ## An example already on the list is not an offer any more. Which ones are
  ## taken is derived state, so it is pushed rather than re-rendered.
  observe({
    used <- Filter(
      Negate(is.null),
      lapply(sets(), function(e) e$example)
    )
    session$sendCustomMessage(
      "builder_used_examples",
      list(ids = as.character(unlist(used)))
    )
  })

  start_load <- function(kind, arg, label) {
    rs <- worker()
    if (is.null(rs)) {
      add_error("The background worker is not ready yet.")
      return()
    }
    add_error(NULL)
    seq_id(seq_id() + 1L)
    id <- paste0("ds", seq_id())
    enqueue(list(
      kind = "load",
      source = kind,
      id = id,
      path = if (identical(kind, "file")) arg else NA_character_,
      example = if (identical(kind, "example")) arg else NULL,
      label = label,
      note = paste0("Loading ", label, "…")
    ))
  }

  observeEvent(input$use_example, {
    ex <- Filter(
      function(e) identical(e$id, input$use_example),
      builder_examples()
    )
    if (!length(ex)) {
      return()
    }
    start_load("example", ex[[1]]$id, ex[[1]]$label)
  })

  ## -- dispatcher: send the next request when the worker is free ----------
  observe({
    q <- queue()
    if (!length(q) || !is.null(pending())) {
      return()
    }
    rs <- worker()
    req(rs)
    nxt <- q[[1]]
    queue(q[-1])
    switch(
      nxt$kind,
      load = if (identical(nxt$source, "file")) {
        builder_session_load(rs, nxt$id, nxt$path)
      } else {
        builder_session_example(rs, nxt$id, nxt$example)
      },
      preview = builder_session_preview(
        rs,
        nxt$id,
        nxt$reduction,
        nxt$group,
        BUILDER_PREVIEW_MAX
      ),
      coords = builder_session_coords(rs, nxt$id, nxt$image),
      align_all = builder_session_section_bounds(
        rs,
        nxt$id,
        nxt$sections,
        nxt$mode,
        nxt$extent_width,
        nxt$extent_height,
        nxt$um_per_px,
        nxt$dx,
        nxt$dy,
        nxt$scale
      ),
      build = builder_session_build(rs, nxt$plan),
      drop = builder_session_drop(rs, nxt$id)
    )
    busy_note(nxt$note)
    pending(nxt)
  })

  ## -- one poller drains whatever the worker was asked to do ---------------
  observe({
    p <- pending()
    if (is.null(p)) {
      return()
    }
    rs <- worker()
    req(rs)
    invalidateLater(300, session)
    got <- builder_session_poll(rs)
    if (is.null(got)) {
      return()
    }
    pending(NULL)
    busy_note(NULL)

    if (!is.null(got$error)) {
      if (identical(p$kind, "build")) {
        result(list(error = got$error))
      } else {
        add_error(got$error)
      }
      return()
    }
    value <- got$value
    if (!is.null(p$request_key)) {
      latest <- isolate(latest_request())
      if (
        !identical(
          p$request_generation,
          latest[[p$request_key]]
        )
      ) {
        return()
      }
    }

    if (identical(p$kind, "load")) {
      if (!is.null(value$error)) {
        add_error(value$error)
        return()
      }
      profile <- value$profile
      entry <- list(
        id = p$id,
        path = p$path,
        ## Which built-in example produced this, so removing it puts the
        ## example back on offer. NULL for anything read from a file.
        example = p$example,
        format = value$format,
        profile = profile,
        dataset_profile = value$dataset_profile,
        ## Level names per grouping variable, in the order the exporter will
        ## produce them -- the keys a configured palette has to match.
        levels = value$levels %||% list(),
        settings = builder_default_settings(profile, unique_name(p$label))
      )
      sets(c(sets(), list(entry)))
      current(p$id)
      just_added(p$id)
      result(NULL)
    } else if (identical(p$kind, "preview")) {
      preview_frame(value)
    } else if (identical(p$kind, "coords")) {
      spatial_coords(value)
    } else if (identical(p$kind, "align_all")) {
      apply_section_bounds(p$id, value, p$picture)
    } else if (identical(p$kind, "build")) {
      result(value)
    }
  })

  ## Take the per-section extents the worker computed and pair each with the
  ## one shared picture.
  apply_section_bounds <- function(id, per_section, a) {
    if (is.null(a) || !length(per_section)) {
      return()
    }
    e <- entry_of(id)
    if (is.null(e)) {
      return()
    }
    paired <- builder_pair_sections(a, per_section)
    imgs <- utils::modifyList(e$settings$images %||% list(), paired)
    short <- names(Filter(function(x) x$outside > 0, paired))
    e$settings$images <- imgs
    replace_entry(e)

    ## Saying "done" when four of five slides have every cell off the image is
    ## how the earlier version of this hid its own bug.
    if (length(short)) {
      showNotification(
        paste0(
          "Applied to ",
          length(per_section),
          " sections, but cells still fall outside the image in: ",
          paste(short, collapse = ", "),
          ". Bounding-box mode usually works best for multi-section objects."
        ),
        type = "warning",
        duration = 10
      )
    } else {
      showNotification(
        paste0(
          "The image was fitted to the coordinates of all ",
          length(per_section),
          " sections."
        ),
        type = "message",
        duration = 5
      )
    }
  }

  output$add_error <- renderUI({
    msg <- add_error()
    if (is.null(msg)) {
      return(NULL)
    }
    div(class = "notice bad", msg)
  })

  ## -- the rail ------------------------------------------------------------
  output$ds_list <- renderUI({
    all <- sets()
    if (!length(all)) {
      return(div(class = "rail-empty", "No datasets yet. Add one below."))
    }
    lapply(seq_along(all), function(i) {
      e <- all[[i]]
      cls <- paste("ds", if (identical(e$id, current())) "is-active" else "")
      ready <- length(e$settings$groups) > 0 &&
        length(e$settings$reductions) > 0 &&
        builder_has_text(e$settings$layer) &&
        builder_has_text(e$settings$nUMI) &&
        builder_has_text(e$settings$nGene)
      ## A container, not a button: the row selects, and a remove button sits
      ## on top of its right end. Nesting one button in another is not markup
      ## a browser will honour.
      div(
        class = cls,
        `data-ds` = e$id,
        tags$button(
          class = "ds-pick builder-pick",
          id = paste0("pick_", e$id),
          `data-ds` = e$id,
          ## The number is the export order, which is also the order of the
          ## switcher in the app this produces.
          span(class = "ds-idx", i),
          span(
            class = "ds-body",
            span(class = "nm", e$settings$name),
            span(
              class = if (ready) "meta" else "meta bad",
              sprintf(
                "%s cells · %s",
                format(e$profile$n_cells, big.mark = ","),
                if (ready) e$format else "Setup incomplete"
              )
            )
          )
        ),
        tags$button(
          class = "ds-del builder-drop",
          title = "Remove this dataset",
          `aria-label` = paste0("Remove ", e$settings$name),
          `data-ds` = e$id,
          icon_svg(ICON_TRASH)
        )
      )
    })
  })

  observeEvent(input$pick, {
    current(input$pick)
    ## Pointing at it makes it no longer new.
    just_added(NULL)
    result(NULL)
  })

  ## -- keep the current entry's settings in step with the inputs -----------
  setting_inputs <- c(
    "name",
    "organism",
    "assay",
    "layer",
    "nUMI",
    "nGene",
    "palette",
    "groups",
    "reductions"
  )
  ## Selections, as opposed to single values. An empty one is a real answer --
  ## "nothing ticked" -- and has to be written back, or unticking the last box
  ## silently keeps the old set.
  multi_inputs <- c("groups", "reductions")

  observe({
    id <- current()
    req(id)
    vals <- lapply(setting_inputs, function(k) input[[k]])
    names(vals) <- setting_inputs
    ## Before the detail pane has rendered for this entry, the inputs still
    ## hold the previous one's values; writing those back would copy settings
    ## across data sets.
    if (is.null(input$rendered_for) || !identical(input$rendered_for, id)) {
      return()
    }
    e <- entry_of(id)
    req(e)
    for (k in setting_inputs) {
      if (!is.null(vals[[k]])) {
        e$settings[[k]] <- vals[[k]]
      } else if (k %in% multi_inputs) {
        e$settings[[k]] <- character()
      }
    }
    replace_entry(e)
  })

  ## Assay-dependent controls must follow the selected assay. A Seurat object
  ## can expose entirely different layers and QC fields in each assay.
  observeEvent(
    input$assay,
    {
      id <- current()
      req(id)
      if (is.null(input$rendered_for) || !identical(input$rendered_for, id)) {
        return()
      }
      entry <- entry_of(id)
      req(entry)
      assay_profile <- entry$profile$assay_profiles[[input$assay]]
      req(assay_profile)
      updateSelectInput(
        session,
        "layer",
        choices = assay_profile$layers,
        selected = if (
          builder_has_text(entry$settings$layer) &&
            entry$settings$layer %in% assay_profile$layers
        ) {
          entry$settings$layer
        } else {
          assay_profile$default_layer
        }
      )
      updateSelectInput(
        session,
        "nUMI",
        choices = assay_profile$nUMI_choices,
        selected = if (builder_has_text(assay_profile$nUMI)) {
          assay_profile$nUMI
        } else {
          character()
        }
      )
      updateSelectInput(
        session,
        "nGene",
        choices = assay_profile$nGene_choices,
        selected = if (builder_has_text(assay_profile$nGene)) {
          assay_profile$nGene
        } else {
          character()
        }
      )
    },
    ignoreInit = TRUE
  )

  ## `analyses` is stamped with the data set it was produced for, so it is
  ## applied by id rather than by whatever is open when it arrives.
  observeEvent(input$analyses, {
    msg <- input$analyses
    req(!is.null(msg$ds))
    e <- entry_of(msg$ds)
    req(e)
    e$settings$analyses <- builder_normalize_analyses(
      as.character(msg$v %||% character()),
      builder_profile_has(e$profile, "marker_genes")
    )
    replace_entry(e)
  })

  ## -- preview --------------------------------------------------------------
  ## Ask the worker for the points whenever the choice changes; it answers with
  ## a few thousand rows regardless of how big the object is.
  observeEvent(
    list(current(), input$preview_reduction, input$preview_group),
    {
      id <- current()
      rs <- worker()
      if (is.null(id) || is.null(rs)) {
        return()
      }
      e <- isolate(entry_of(id))
      if (is.null(e)) {
        return()
      }
      red <- input$preview_reduction %||%
        (if (length(e$settings$reductions)) e$settings$reductions[1] else NULL)
      if (is.null(red)) {
        return()
      }
      preview_frame(NULL)
      enqueue(list(
        kind = "preview",
        replaces = "preview",
        id = id,
        reduction = red,
        group = input$preview_group,
        note = "Rendering preview…"
      ))
    },
    ignoreInit = FALSE
  )

  ## -- group colours --------------------------------------------------------
  ## Stored per data set as settings$colors[[variable]] = c(level = "#hex"),
  ## which is exactly the shape createShinyApp(colors = ) wants, so nothing has
  ## to be reshaped at build time and nothing can be reshaped wrongly.
  ##
  ## Levels for the variable the preview is coloured by, in export order.
  preview_levels <- reactive({
    id <- current()
    e <- if (is.null(id)) NULL else entry_of(id)
    g <- input$preview_group
    if (is.null(e) || is.null(g) || !nzchar(g)) {
      return(character())
    }
    (e$levels %||% list())[[g]] %||% character()
  })

  preview_colors <- reactive({
    lv <- preview_levels()
    if (!length(lv)) {
      return(NULL)
    }
    id <- current()
    entry <- if (is.null(id)) NULL else entry_of(id)
    if (is.null(entry)) {
      return(NULL)
    }
    g <- input$preview_group
    builder_level_colors(
      lv,
      entry$settings$palette %||% "cerebro",
      (entry$settings$color_overrides %||% list())[[g]]
    )
  })

  ## The browser sends the dataset, grouping variable and level with every
  ## swatch edit. No value can leak into whichever dataset happens to be open
  ## when a delayed input event arrives.
  observeEvent(input$swatch_change, {
    msg <- input$swatch_change
    req(msg$ds, msg$group, msg$level, msg$value)
    entry <- entry_of(msg$ds)
    req(entry)
    overrides <- entry$settings$color_overrides %||% list()
    per_group <- overrides[[msg$group]] %||%
      stats::setNames(character(), character())
    per_group[[msg$level]] <- msg$value
    overrides[[msg$group]] <- per_group
    entry$settings$color_overrides <- overrides
    replace_entry(entry)
  })

  observeEvent(input$reset_colors, {
    id <- current()
    req(id)
    entry <- entry_of(id)
    req(entry)
    overrides <- entry$settings$color_overrides %||% list()
    overrides[[input$preview_group]] <- NULL
    entry$settings$color_overrides <- overrides
    entry$settings$palette <- "cerebro"
    replace_entry(entry)
    updateSelectInput(session, "palette", selected = "cerebro")
  })

  output$palette_note <- renderUI({
    id <- current()
    entry <- if (is.null(id)) NULL else entry_of(id)
    req(entry)
    pal <- Filter(
      function(p) identical(p$id, entry$settings$palette %||% "cerebro"),
      builder_palettes()
    )
    if (!length(pal)) {
      return(NULL)
    }
    p(class = "hint", style = "margin-top:-.5rem", pal[[1]]$note)
  })

  output$color_swatches <- renderUI({
    lv <- preview_levels()
    if (!length(lv)) {
      return(p(
        class = "hint",
        "This grouping variable has no colourable values."
      ))
    }
    ## A variable with hundreds of levels is not something anyone edits one
    ## swatch at a time, and 200 colour inputs would make the page unusable.
    if (length(lv) > BUILDER_SWATCH_MAX) {
      return(p(
        class = "hint",
        sprintf(
          "%s has %d values, so individual swatches are hidden. Choose a palette instead.",
          input$preview_group,
          length(lv)
        )
      ))
    }
    cols <- preview_colors()
    div(
      class = "swatches",
      lapply(seq_along(lv), function(i) {
        tags$label(
          class = "swatch",
          tags$input(
            type = "color",
            class = "swatch-input",
            value = unname(cols[[lv[i]]]),
            `data-ds` = current(),
            `data-group` = input$preview_group,
            `data-level` = lv[i]
          ),
          tags$span(class = "swatch-name", lv[i])
        )
      })
    )
  })

  output$preview_plot <- plotly::renderPlotly({
    df <- preview_frame()
    req(!is.null(df))
    plt <- builder_preview_plot(df, preview_colors())
    req(!is.null(plt))
    plt
  })

  ## -- optional analyses ----------------------------------------------------
  output$analysis_choices <- renderUI({
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    chosen <- e$settings$analyses %||% character()

    tags$div(
      class = "steps",
      lapply(builder_analysis_steps(), function(step) {
        blocked <- builder_step_blocked(step, e$profile, chosen)
        tags$label(
          class = paste("step-row", if (!is.null(blocked)) "is-blocked"),
          tags$input(
            class = "builder-analysis",
            type = "checkbox",
            name = "analyses_box",
            value = step$id,
            `data-ds` = id,
            checked = if (step$id %in% chosen) "checked",
            disabled = if (!is.null(blocked)) "disabled"
          ),
          tags$span(class = "step-label", step$label),
          tags$span(
            class = paste(
              "pill",
              if (isTRUE(step$network)) "cost-net" else "cost"
            ),
            step$cost
          ),
          tags$span(
            class = "step-note",
            if (!is.null(blocked)) blocked else step$note
          )
        )
      })
    )
  })

  ## -- supplementary tables -------------------------------------------------
  observeEvent(input$add_table, {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    got <- builder_read_table(
      trimws(input$table_path %||% ""),
      trimws(input$table_name %||% "")
    )
    if (!is.null(got$error)) {
      showNotification(got$error, type = "error", duration = 8)
      return()
    }
    e$settings$tables[[got$name]] <- got
    replace_entry(e)
    updateTextInput(session, "table_path", value = "")
    updateTextInput(session, "table_name", value = "")
  })

  observeEvent(input$drop_table, {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    e$settings$tables[[input$drop_table]] <- NULL
    replace_entry(e)
  })

  output$table_list <- renderUI({
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    tabs <- e$settings$tables
    if (!length(tabs)) {
      return(p(class = "hint", "No supplementary tables yet."))
    }
    tags$ul(
      class = "chip-list",
      lapply(names(tabs), function(nm) {
        tags$li(
          tags$b(nm),
          tags$span(
            class = "hint",
            sprintf(
              " %d rows × %d columns",
              nrow(tabs[[nm]]$table),
              ncol(tabs[[nm]]$table)
            )
          ),
          tags$button(
            class = "btn btn-quiet builder-table-drop",
            style = "padding:.1rem .4rem;min-height:auto",
            `data-name` = nm,
            "Remove"
          )
        )
      })
    )
  })

  ## -- histology background -------------------------------------------------
  ## The raw image is decoded once and kept as an array; the sliders then only
  ## re-encode it, so dragging is cheap and never loses quality by re-reading a
  ## picture that has already been resampled.
  raw_image <- reactiveVal(NULL)

  output$has_image <- reactive(!is.null(raw_image()))
  outputOptions(output, "has_image", suspendWhenHidden = FALSE)

  ## Which tissue section the alignment controls currently describe. An object
  ## can hold several, each with its own coordinates and its own slide.
  active_slice <- reactiveVal(NULL)

  observeEvent(current(), {
    raw_image(NULL)
    spatial_coords(NULL)
    id <- current()
    rs <- worker()
    e <- isolate(entry_of(id))
    if (is.null(id) || is.null(rs) || is.null(e)) {
      active_slice(NULL)
      return()
    }
    if (!length(e$profile$images)) {
      active_slice(NULL)
      return()
    }
    active_slice(e$profile$images[1])
    enqueue(list(
      kind = "coords",
      id = id,
      image = e$profile$images[1],
      replaces = "coords",
      note = "Loading spatial coordinates…"
    ))
  })

  ## Switching section re-reads that section's coordinates and drops the
  ## in-progress alignment, which described a different slide.
  observeEvent(input$active_slice, {
    id <- current()
    req(id)
    e <- isolate(entry_of(id))
    req(e)
    nm <- input$active_slice
    if (!nzchar(nm) || identical(nm, isolate(active_slice()))) {
      return()
    }
    active_slice(nm)
    raw_image(NULL)
    spatial_coords(NULL)
    enqueue(list(
      kind = "coords",
      id = id,
      image = nm,
      replaces = "coords",
      note = paste0("Loading coordinates for ", nm, "…")
    ))
  })

  observeEvent(input$attach_image, {
    img <- builder_read_image(trimws(input$image_path %||% ""))
    if (!is.null(img$error)) {
      showNotification(img$error, type = "error", duration = 8)
      return()
    }
    raw_image(img)
    updateSliderInput(session, "img_dx", value = 0)
    updateSliderInput(session, "img_dy", value = 0)
    updateSliderInput(session, "img_scale", value = 1)
    updateSliderInput(session, "img_rotate", value = 0)
  })

  observeEvent(input$reset_align, {
    updateSliderInput(session, "img_dx", value = 0)
    updateSliderInput(session, "img_dy", value = 0)
    updateSliderInput(session, "img_scale", value = 1)
    updateSliderInput(session, "img_rotate", value = 0)
    updateCheckboxInput(session, "image_flip", value = FALSE)
    updateCheckboxInput(session, "image_flip_x", value = FALSE)
  })

  ## Slider ranges have to be in the data's own units, or "left a bit" means
  ## nothing on an object whose coordinates run to 20,000.
  observeEvent(spatial_coords(), {
    co <- spatial_coords()
    req(!is.null(co))
    ## Round to something a person can read: coordinate spans are arbitrary
    ## reals and an unrounded slider shows -93.3687755886931 as its label.
    nice <- function(v) {
      if (!is.finite(v) || v <= 0) {
        return(1)
      }
      signif(v, 2)
    }
    span_x <- nice(diff(range(co$x, na.rm = TRUE)))
    span_y <- nice(diff(range(co$y, na.rm = TRUE)))
    updateSliderInput(
      session,
      "img_dx",
      min = -span_x,
      max = span_x,
      value = 0,
      step = nice(span_x / 200)
    )
    updateSliderInput(
      session,
      "img_dy",
      min = -span_y,
      max = span_y,
      value = 0,
      step = nice(span_y / 200)
    )
  })

  ## Encoding is the expensive part. Translation and scale only alter the
  ## extent, so they must not decode, rotate and base64-encode the image again.
  encoded_image <- reactive({
    img <- raw_image()
    req(!is.null(img))
    enc <- builder_encode_image(
      img$array,
      max_px = input$image_max_px %||% 1400,
      flip_y = isTRUE(input$image_flip),
      flip_x = isTRUE(input$image_flip_x),
      rotate = input$img_rotate %||% 0
    )
    if (!is.null(enc$error)) {
      return(list(error = enc$error))
    }
    enc
  })

  image_base_bounds <- reactive({
    enc <- encoded_image()
    co <- spatial_coords()
    req(!is.null(co))
    if (!is.null(enc$error)) {
      return(enc)
    }
    b0 <- builder_image_bounds(
      input$image_bounds_mode %||% "pixels",
      list(co$x, co$y),
      enc,
      um_per_px = input$image_um %||% 1
    )
    if (!is.null(b0$error)) {
      return(list(error = b0$error))
    }
    b0
  })

  ## What the sliders currently describe: a cached encoded picture and a
  ## lightweight extent adjustment.
  aligned <- reactive({
    enc <- encoded_image()
    b0 <- image_base_bounds()
    co <- spatial_coords()
    req(!is.null(co))
    if (!is.null(enc$error)) {
      return(enc)
    }
    if (!is.null(b0$error)) {
      return(b0)
    }
    bounds <- builder_adjust_bounds(
      b0,
      dx = input$img_dx %||% 0,
      dy = input$img_dy %||% 0,
      scale = input$img_scale %||% 1
    )
    cover <- builder_bounds_cover(bounds, list(co$x, co$y))
    list(enc = enc, bounds = bounds, cover = cover)
  })

  output$overlay_plot <- plotly::renderPlotly({
    a <- aligned()
    req(is.null(a$error))
    plt <- builder_overlay_plot(spatial_coords(), a$enc$uri, a$bounds)
    req(!is.null(plt))
    plt
  })

  ## What the sliders currently describe, as one section's stored entry.
  current_alignment <- function() {
    a <- aligned()
    if (is.null(a) || !is.null(a$error)) {
      return(a)
    }
    list(
      uri = a$enc$uri,
      bounds = a$bounds,
      bytes = a$enc$bytes,
      width = a$enc$width,
      height = a$enc$height,
      source_width = a$enc$source_width,
      source_height = a$enc$source_height,
      extent_width = a$enc$extent_width,
      extent_height = a$enc$extent_height,
      display_width = a$enc$display_width,
      display_height = a$enc$display_height,
      outside = a$cover$outside,
      total = a$cover$total
    )
  }

  observeEvent(input$apply_align, {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    nm <- active_slice()
    req(!is.null(nm))
    a <- current_alignment()
    if (!is.null(a$error)) {
      showNotification(a$error, type = "error", duration = 8)
      return()
    }
    imgs <- e$settings$images %||% list()
    imgs[[nm]] <- a
    e$settings$images <- imgs
    replace_entry(e)
    showNotification(
      paste0("Alignment saved for section “", nm, "”."),
      type = "message",
      duration = 4
    )
  })

  ## Sections cut from one block often share a slide scan. Doing the alignment
  ## by hand for twelve sections is the kind of chore that makes people skip
  ## backgrounds entirely -- but "same slide" is not "same extent". Sections sit
  ## at different offsets in the coordinate space, so the extent is re-derived
  ## per section in the worker; only the picture is shared.
  observeEvent(input$apply_align_all, {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    a <- current_alignment()
    if (is.null(a)) {
      return()
    }
    if (!is.null(a$error)) {
      showNotification(a$error, type = "error", duration = 8)
      return()
    }
    ## The encoded picture is kept here; sending 50 kB of base64 to the worker
    ## and back once per section would be pure waste.
    enqueue(list(
      kind = "align_all",
      id = id,
      sections = e$profile$images,
      mode = input$image_bounds_mode %||% "pixels",
      extent_width = a$extent_width,
      extent_height = a$extent_height,
      um_per_px = input$image_um %||% 1,
      dx = input$img_dx %||% 0,
      dy = input$img_dy %||% 0,
      scale = input$img_scale %||% 1,
      picture = a,
      replaces = "align_all",
      note = paste0(
        "Fitting the image to ",
        length(e$profile$images),
        " sections…"
      )
    ))
  })

  observeEvent(input$drop_image, {
    id <- current()
    req(id)
    e <- entry_of(id)
    req(e)
    nm <- active_slice()
    imgs <- e$settings$images %||% list()
    if (!is.null(nm)) {
      imgs[[nm]] <- NULL
    }
    e$settings$images <- imgs
    replace_entry(e)
    raw_image(NULL)
  })

  output$image_state <- renderUI({
    a <- if (is.null(raw_image())) NULL else aligned()
    id <- current()
    e <- if (is.null(id)) NULL else entry_of(id)
    nm <- active_slice()
    stored <- if (is.null(e)) list() else (e$settings$images %||% list())
    saved <- if (is.null(nm)) NULL else stored[[nm]]
    n_slices <- if (is.null(e)) 0L else length(e$profile$images)

    tagList(
      if (!is.null(a) && !is.null(a$error)) div(class = "notice bad", a$error),
      if (!is.null(a) && is.null(a$error)) {
        div(
          class = if (a$cover$outside > 0) "notice warn" else "notice ok",
          sprintf(
            "%d × %d px, %.2f MB. %s",
            a$enc$width,
            a$enc$height,
            a$enc$bytes / 1048576,
            if (a$cover$outside > 0) {
              sprintf(
                "%d/%d cells fall outside the image.",
                a$cover$outside,
                a$cover$total
              )
            } else {
              "All cells fall inside the image."
            }
          )
        )
      },
      if (!is.null(saved)) {
        div(
          style = "margin-top:.5rem;display:flex;gap:.5rem;align-items:center",
          span(
            class = "pill on",
            if (n_slices > 1) {
              paste0("Alignment saved for “", nm, "”")
            } else {
              "Alignment saved"
            }
          ),
          actionButton("drop_image", "Remove", class = "btn btn-quiet")
        )
      } else if (is.null(raw_image())) {
        p(class = "hint", "No background image yet.")
      },
      ## With several sections it is not otherwise visible which of them are
      ## still bare -- and a section with no image loses the whole background
      ## picker in the viewer, so the controls appear and disappear as the user
      ## switches. Worth stating rather than discovering.
      if (n_slices > 1) {
        done <- intersect(e$profile$images, names(stored))
        missing <- setdiff(e$profile$images, names(stored))
        div(
          class = "hint",
          style = "margin-top:.5rem",
          if (length(missing)) {
            paste0(
              length(done),
              "/",
              n_slices,
              " sections have images. Missing: ",
              paste(missing, collapse = ", ")
            )
          } else {
            paste0("All ", n_slices, " sections have background images.")
          }
        )
      }
    )
  })

  ## -- what the last build produced ---------------------------------------
  output$result_card <- renderUI({
    r <- result()
    if (is.null(r)) {
      return(NULL)
    }
    div(
      class = "card result-card",
      h2("Latest build"),
      if (!is.null(r$error)) div(class = "notice bad", r$error),
      if (length(r$built)) {
        tagList(
          div(
            class = "notice ok",
            sprintf(
              "%d dataset%s published to %s",
              length(r$built),
              if (length(r$built) == 1L) "" else "s",
              dirname(r$built[1])
            )
          ),
          tags$ul(
            style = "margin:.6rem 0 0;padding-left:1.2rem",
            lapply(seq_along(r$built), function(i) {
              tags$li(
                tags$b(r$labels[i]),
                " — ",
                tags$code(basename(r$built[i])),
                sprintf(" (%.2f MB)", file.size(r$built[i]) / 1048576)
              )
            })
          )
        )
      },
      if (!is.null(r$app_dir)) {
        tagList(
          p(class = "hint", style = "margin-top:.8rem", "Run this app:"),
          tags$pre(paste0('shiny::runApp("', r$app_dir, '")'))
        )
      },
      if (length(r$analysis_log)) {
        tagList(
          p(class = "hint", style = "margin-top:.8rem", "Analysis steps:"),
          tags$ul(
            style = "margin:.2rem 0 0;padding-left:1.2rem",
            lapply(r$analysis_log, function(x) tags$li(class = "hint", x))
          )
        )
      },
      if (length(r$failures)) {
        tagList(
          div(class = "notice bad", style = "margin-top:.7rem", "Failures:"),
          tags$ul(
            style = "margin:.4rem 0 0;padding-left:1.2rem",
            lapply(r$failures, tags$li)
          )
        )
      }
    )
  })

  ## -- the detail pane -----------------------------------------------------
  output$detail <- renderUI({
    id <- current()
    ## Depend on *which* data set is shown, never on its settings. The sync
    ## observer writes every keystroke back into the entry, so reacting to the
    ## contents would rebuild the whole form on each character -- losing focus,
    ## resetting numeric fields, and making the value the server sees lag the
    ## one on screen.
    e <- isolate(entry_of(id))
    if (is.null(id)) {
      return(div(
        class = "card span-12",
        h2("Get started"),
        p(
          "Choose a Seurat object from the left, or explore a built-in example."
        ),
        p(class = "hint", builder_format_summary()),
        p(
          class = "hint",
          "Add multiple objects to bundle them into one app with a dataset switcher."
        )
      ))
    }
    req(e)
    p <- e$profile
    s <- e$settings
    assay_profile <- p$assay_profiles[[s$assay]] %||%
      list(
        layers = p$layers,
        default_layer = p$default_layer,
        nUMI_choices = p$nUMI,
        nGene_choices = p$nGene,
        nUMI = p$nUMI,
        nGene = p$nGene
      )
    selected_qc <- function(value, choices, default) {
      if (builder_has_text(value) && value %in% choices) {
        value
      } else if (builder_has_text(default) && default %in% choices) {
        default
      } else {
        NULL
      }
    }

    ## Was this data set just read, or did the user click over to it? The
    ## answer has to travel as an attribute in the rendered HTML rather than a
    ## script: builder.js reads it from a MutationObserver, which runs before
    ## the browser paints, and a <script> in the fragment would be one paint
    ## too late to set the animation's starting state.
    fresh <- identical(id, isolate(just_added()))

    tagList(
      div(
        class = "card span-12",
        h2("Object", if (fresh) span(class = "pill new", "Just added")),
        div(
          class = "facts",
          div(
            span(class = "k", "Cells "),
            span(class = "v", format(p$n_cells, big.mark = ","))
          ),
          div(
            span(class = "k", "Genes "),
            span(class = "v", format(p$n_genes, big.mark = ","))
          ),
          div(span(class = "k", "Format "), span(class = "v", e$format)),
          div(
            span(class = "k", "Spatial "),
            span(
              class = "v",
              if (length(p$images)) paste(p$images, collapse = ", ") else "None"
            )
          )
        ),
        p(
          class = "hint",
          style = "margin-top:.6rem;word-break:break-all",
          ## An example was generated in the worker and has no file behind it.
          ## Printing its absent path as "NA" reads like a failed read.
          if (is.null(e$path) || is.na(e$path)) {
            "Built-in example; no source file"
          } else {
            e$path
          }
        ),
        hr(),
        div(
          class = "hint",
          style = "margin-bottom:.4rem",
          "Content already present:"
        ),
        div(
          class = "pills",
          lapply(p$extras, function(x) {
            span(class = paste("pill", if (x$found) "on" else "off"), x$label)
          })
        ),
        p(
          class = "hint",
          "Muted items are absent, so their pages will be hidden."
        )
      ),

      div(
        class = "card span-4",
        h2("Identity"),
        div(
          class = "field-grid",
          textInput("name", "Dataset name", value = s$name, width = "100%"),
          selectInput(
            "organism",
            "Organism",
            choices = c(
              "Human (hg)" = "hg",
              "Mouse (mm)" = "mm",
              "Other" = "other"
            ),
            selected = s$organism,
            width = "100%"
          )
        ),
        p(class = "hint", "This name becomes the label in the app switcher.")
      ),

      div(
        class = "card span-8",
        h2("Expression"),
        div(
          class = "field-grid expression-fields",
          selectInput(
            "assay",
            "Assay",
            choices = p$assays,
            selected = s$assay,
            width = "100%"
          ),
          selectInput(
            "layer",
            "Layer / slot",
            choices = assay_profile$layers,
            selected = if (
              builder_has_text(s$layer) &&
                s$layer %in% assay_profile$layers
            ) {
              s$layer
            } else {
              assay_profile$default_layer
            },
            width = "100%"
          ),
          selectInput(
            "nUMI",
            "UMI / count field",
            choices = assay_profile$nUMI_choices,
            selected = selected_qc(
              s$nUMI,
              assay_profile$nUMI_choices,
              assay_profile$nUMI
            ),
            width = "100%"
          ),
          selectInput(
            "nGene",
            "Feature / gene field",
            choices = assay_profile$nGene_choices,
            selected = selected_qc(
              s$nGene,
              assay_profile$nGene_choices,
              assay_profile$nGene
            ),
            width = "100%"
          )
        ),
        p(
          class = "hint",
          "Only complete, full-cell layers are offered. Split Seurat v5 layers are joined safely before export."
        )
      ),

      div(
        class = "card span-8 cols-2",
        h2("Grouping variables"),
        checkboxGroupInput(
          "groups",
          NULL,
          choices = p$group_candidates,
          selected = s$groups
        ),
        if (length(p$group_struck)) {
          p(
            class = "hint",
            paste0("Excluded: ", paste(p$group_struck, collapse = "; "))
          )
        },
        p(
          class = "hint",
          "Used for colouring and filtering. Select at least one."
        )
      ),

      div(
        class = "card span-4",
        h2("Reductions"),
        if (length(p$reductions)) {
          tagList(
            checkboxGroupInput(
              "reductions",
              NULL,
              choices = p$reductions,
              selected = s$reductions
            ),
            p(
              class = "hint",
              paste(
                "Exactly one PCA alone is exported as a warned fallback.",
                "When a non-PCA reduction is selected, PCA is omitted."
              )
            )
          )
        } else {
          div(class = "notice bad", "This object has no reductions to display.")
        }
      ),

      div(
        class = "card span-8",
        h2("Preview & colours"),
        div(
          class = "row2",
          selectInput(
            "preview_reduction",
            "Reduction",
            choices = p$reductions,
            selected = if (length(s$reductions)) s$reductions[1] else NULL,
            width = "100%"
          ),
          selectInput(
            "preview_group",
            "Colour by",
            choices = unname(p$group_candidates),
            selected = if (length(s$groups)) s$groups[1] else NULL,
            width = "100%"
          )
        ),
        plotly::plotlyOutput("preview_plot", height = "340px"),
        p(
          class = "hint",
          paste0(
            "Datasets above ",
            format(BUILDER_PREVIEW_MAX, big.mark = ","),
            " cells are deterministically sampled, so the same object produces the same preview."
          )
        ),
        hr(),
        div(
          class = "row2",
          selectInput(
            "palette",
            "Palette",
            choices = stats::setNames(
              vapply(builder_palettes(), function(p) p$id, ""),
              vapply(builder_palettes(), function(p) p$label, "")
            ),
            selected = s$palette %||% "cerebro",
            width = "100%"
          ),
          div(
            style = "align-self:end;padding-bottom:.85rem",
            actionButton("reset_colors", "Reset colours", class = "btn")
          )
        ),
        uiOutput("palette_note"),
        uiOutput("color_swatches"),
        p(
          class = "hint",
          paste0(
            "Colours are bundled with “",
            s$name,
            "”. A standalone .crb does not carry them; the viewer can still adjust colours at runtime."
          )
        )
      ),

      div(
        class = "card-stack span-4",
        div(
          class = "card",
          h2("Pre-export analyses"),
          uiOutput("analysis_choices"),
          p(
            class = "hint",
            "Selected results are embedded to enable the matching pages."
          )
        ),
        div(
          class = "card",
          h2("Supplementary tables"),
          div(
            class = "field-grid",
            textInput(
              "table_path",
              "CSV / TSV path",
              width = "100%",
              placeholder = "/path/to/de_results.csv"
            ),
            textInput(
              "table_name",
              "Display name (optional)",
              width = "100%"
            )
          ),
          actionButton("add_table", "Add table", class = "btn"),
          uiOutput("table_list")
        )
      ),

      if (length(p$images)) {
        div(
          class = "card span-12",
          h2("Histology background"),
          ## Only worth a control when there is a choice to make. One section is
          ## the common case and should look exactly as it did before.
          if (length(p$images) > 1) {
            tagList(
              selectInput(
                "active_slice",
                paste0("Tissue section (", length(p$images), " total)"),
                choices = p$images,
                selected = isolate(active_slice()) %||% p$images[1],
                width = "100%"
              ),
              p(
                class = "hint",
                style = "margin-top:-.4rem;margin-bottom:.8rem",
                paste0(
                  "Each section has its own coordinates and slide. Images and alignment are saved per section."
                )
              )
            )
          },
          div(
            class = "row2",
            textInput(
              "image_path",
              "PNG / JPEG path",
              width = "100%",
              placeholder = "/path/to/he.png"
            ),
            selectInput(
              "image_bounds_mode",
              "Image extent",
              choices = c(
                "Cell coordinates are image pixels" = "pixels",
                "Cell coordinates use physical units" = "physical",
                "Fit to the cell bounding box" = "bbox"
              ),
              width = "100%"
            )
          ),
          div(
            class = "row2",
            numericInput(
              "image_um",
              "Physical units per pixel",
              value = 1,
              min = 0.0001,
              step = 0.05,
              width = "100%"
            ),
            numericInput(
              "image_max_px",
              "Maximum image edge (px)",
              value = 1400,
              min = 200,
              max = 4000,
              step = 100,
              width = "100%"
            )
          ),
          actionButton("attach_image", "Load image", class = "btn"),
          uiOutput("image_state"),

          conditionalPanel(
            condition = "output.has_image",
            hr(),
            div(
              class = "hint",
              style = "margin-bottom:.5rem",
              "Adjust the image until tissue and cells align:"
            ),
            div(
              class = "row2",
              sliderInput(
                "img_dx",
                "Horizontal offset",
                min = -1,
                max = 1,
                value = 0,
                step = 0.01,
                width = "100%"
              ),
              sliderInput(
                "img_dy",
                "Vertical offset",
                min = -1,
                max = 1,
                value = 0,
                step = 0.01,
                width = "100%"
              )
            ),
            div(
              class = "row2",
              sliderInput(
                "img_scale",
                "Scale",
                min = 0.2,
                max = 3,
                value = 1,
                step = 0.01,
                width = "100%"
              ),
              sliderInput(
                "img_rotate",
                "Rotation (degrees)",
                min = -180,
                max = 180,
                value = 0,
                step = 1,
                width = "100%"
              )
            ),
            div(
              class = "row2",
              checkboxInput("image_flip", "Flip vertically", value = FALSE),
              checkboxInput("image_flip_x", "Flip horizontally", value = FALSE)
            ),
            plotly::plotlyOutput("overlay_plot", height = "360px"),
            div(
              style = "margin-top:.6rem;display:flex;gap:.5rem;flex-wrap:wrap",
              actionButton(
                "apply_align",
                if (length(p$images) > 1) {
                  "Save this section"
                } else {
                  "Save alignment"
                },
                class = "btn btn-primary"
              ),
              if (length(p$images) > 1) {
                actionButton(
                  "apply_align_all",
                  paste0("Apply to all ", length(p$images), " sections"),
                  class = "btn"
                )
              },
              actionButton("reset_align", "Reset", class = "btn")
            )
          ),
          p(
            class = "hint",
            paste0(
              "Rotation redraws the image; translation and scale only change its extent, preserving image quality."
            )
          )
        )
      },

      div(
        class = "detail-footer",
        actionButton(
          "remove",
          tagList(icon_svg(ICON_TRASH), " Remove this dataset"),
          class = "btn btn-quiet"
        )
      ),

      ## Kept last so the cards above are this container's first children --
      ## the staggered entrance is addressed with :nth-child, and a marker in
      ## front of them would shift every index by one.
      div(
        class = "pane-flag",
        style = "display:none",
        `data-ds` = id,
        `data-new` = if (fresh) "1" else "0"
      )
    )
  })

  ## -- the action bar ------------------------------------------------------
  ready_report <- reactive({
    all <- sets()
    if (!length(all)) {
      return(list(ok = FALSE, msg = "No datasets yet."))
    }
    bad <- Filter(
      function(e) {
        !length(e$settings$groups) ||
          !length(e$settings$reductions) ||
          !builder_has_text(e$settings$layer) ||
          !builder_has_text(e$settings$nUMI) ||
          !builder_has_text(e$settings$nGene)
      },
      all
    )
    if (length(bad)) {
      return(list(
        ok = FALSE,
        msg = paste0(
          length(bad),
          " dataset",
          if (length(bad) == 1L) " is" else "s are",
          " missing a required expression, QC, group or reduction setting."
        )
      ))
    }
    names_used <- trimws(vapply(
      all,
      function(e) e$settings$name,
      character(1)
    ))
    if (any(!nzchar(names_used))) {
      return(list(ok = FALSE, msg = "Every dataset needs a name."))
    }
    if (anyDuplicated(names_used)) {
      return(list(ok = FALSE, msg = "Dataset names must be unique."))
    }
    list(
      ok = TRUE,
      msg = paste0(
        length(all),
        " dataset",
        if (length(all) == 1L) "" else "s",
        " · ",
        format(
          sum(vapply(all, function(e) e$profile$n_cells, numeric(1))),
          big.mark = ","
        ),
        " cells"
      )
    )
  })

  output$actionbar <- renderUI({
    if (!length(sets())) {
      return(NULL)
    }
    rep <- ready_report()
    make_app_control <- builder_app_control(
      app_capability,
      current_value = isolate(input$make_app)
    )
    div(
      class = "actionbar",
      div(
        class = "inner",
        div(
          class = "grow output-field",
          tags$label(
            class = "visually-hidden",
            `for` = "out_dir",
            "Output directory"
          ),
          textInput(
            "out_dir",
            NULL,
            width = "100%",
            value = isolate(input$out_dir) %||%
              file.path(path.expand("~"), "cerebro")
          )
        ),
        div(class = "summary", rep$msg),
        make_app_control,
        checkboxInput(
          "overwrite",
          "Replace existing outputs",
          value = FALSE,
          width = "auto"
        ),
        actionButton(
          "build",
          "Build",
          class = "btn btn-action",
          disabled = if (!rep$ok) "disabled"
        )
      )
    )
  })

  ## -- build ---------------------------------------------------------------
  ## The whole export runs in the worker: analyses, matrix write, bundle. This
  ## process only sends a plan and waits for the report, so the page keeps
  ## answering while a marker-gene run takes its minutes.
  observeEvent(input$build, {
    rs <- worker()
    req(rs)
    all <- sets()
    req(length(all) > 0)
    out_dir <- trimws(input$out_dir %||% "")
    result(NULL)

    plan <- builder_make_plan(
      all,
      out_dir,
      make_app = isTRUE(input$make_app),
      overwrite = isTRUE(input$overwrite)
    )
    if (!is.null(plan$error)) {
      result(list(error = plan$error))
      return()
    }
    if (length(plan$existing_targets) && !isTRUE(plan$overwrite)) {
      result(list(
        error = paste0(
          "These outputs already exist: ",
          paste(basename(plan$existing_targets), collapse = ", "),
          ". Enable “Replace existing outputs” to publish atomically over them."
        )
      ))
      return()
    }
    enqueue(list(
      kind = "build",
      plan = plan,
      note = paste0(
        "Building ",
        length(all),
        " dataset",
        if (length(all) == 1L) "" else "s",
        "…"
      )
    ))
  })

  ## Removing a data set has to free it in the worker too, or "remove" only
  ## hides it while the memory stays taken.
  remove_dataset <- function(id) {
    if (is.null(id) || !nzchar(id)) {
      return()
    }
    enqueue(list(kind = "drop", id = id, note = "Releasing memory…"))
    all <- Filter(function(e) !identical(e$id, id), sets())
    sets(all)
    if (identical(just_added(), id)) {
      just_added(NULL)
    }
    ## Only move the form if the data set it was describing is the one that
    ## went away; removing some other row should leave the user where they are.
    if (identical(current(), id)) {
      current(if (length(all)) all[[1]]$id else NULL)
      result(NULL)
    }
  }

  ## The button on the row -- works on any data set, not just the open one.
  observeEvent(input$drop_ds, remove_dataset(input$drop_ds))

  ## The button at the foot of the form.
  observeEvent(input$remove, remove_dataset(current()))

  output$busy <- renderUI({
    note <- busy_note()
    if (is.null(note)) {
      return(NULL)
    }
    div(class = "busy", span(class = "spinner"), note)
  })
}

shinyApp(ui, server)
