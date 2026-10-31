groups_charts_env <- function() {
  scope <- new.env(parent = asNamespace("shiny"))
  sys.source(viewer_test_path("plotting_functions.R"), scope)
  utilities <- viewer_test_path("utility_functions.R")
  for (name in c("formatTableAB", "calculateTableAB")) {
    eval(viewer_r_assignment(utilities, name), scope)
  }
  scope
}

groups_charts_metadata <- function() {
  data.frame(
    sample = factor(
      c("s1", "s1", "s2", "s2", "s2", "s3", NA),
      levels = c("s1", "s2", "s3", "unused")
    ),
    cluster = factor(
      c("c2", "c1", "c1", "c1", "c2", "c2", "c1"),
      levels = c("c1", "c2")
    )
  )
}

test_that("bar payloads hold the composition table counts and fractions", {
  scope <- groups_charts_env()
  metadata <- groups_charts_metadata()
  colors <- c(c2 = "#00ff00", c1 = "#ff0000", other = "#0000ff")
  for (percent in c(FALSE, TRUE)) {
    long <- scope$calculateTableAB(
      metadata,
      "sample",
      "cluster",
      "long",
      percent
    )
    wide <- scope$calculateTableAB(
      metadata,
      "sample",
      "cluster",
      "wide",
      percent
    )
    bar <- scope$cerebroBarChartData(long, "sample", "cluster", colors, percent)
    expect_identical(as.vector(bar$categories), c("s1", "s2", "s3"))
    expect_identical(as.vector(bar$series), c("c1", "c2"))
    expect_identical(as.vector(bar$colors), c("#ff0000", "#00ff00"))
    expect_identical(bar$percent, percent)
    rows <- match(bar$categories, as.character(wide$sample))
    for (i in seq_along(bar$series)) {
      expect_equal(
        as.vector(bar$values[[i]]),
        as.numeric(wide[[bar$series[[i]]]][rows]),
        tolerance = 1e-6
      )
    }
  }
  expect_identical(bar$y_title, "Percent of cells")
  missing <- scope$cerebroBarChartData(long, "sample", "cluster", NULL, FALSE)
  expect_identical(as.vector(missing$colors), c("#898989", "#898989"))
})

test_that("Sankey links add up to the cells counted in both groups", {
  scope <- groups_charts_env()
  metadata <- groups_charts_metadata()
  long <- scope$calculateTableAB(
    metadata,
    "sample",
    "cluster",
    "long",
    FALSE
  )
  sankey <- scope$cerebroSankeyData(
    long,
    "sample",
    "cluster",
    c(s1 = "#111111", s2 = "#222222", s3 = "#333333"),
    c(c1 = "#ff0000", c2 = "#00ff00")
  )
  expect_identical(as.vector(sankey$sources), c("s1", "s2", "s3"))
  expect_identical(as.vector(sankey$targets), c("c1", "c2"))
  expect_identical(
    as.vector(sankey$source_colors),
    c("#111111", "#222222", "#333333")
  )
  complete <- metadata[stats::complete.cases(metadata), ]
  expect_equal(sum(sankey$link_value), nrow(complete))
  by_source <- tapply(
    sankey$link_value,
    sankey$sources[sankey$link_source + 1L],
    sum
  )
  expect_equal(
    as.vector(by_source[c("s1", "s2", "s3")]),
    as.vector(table(droplevels(complete$sample)))
  )
  by_target <- tapply(
    sankey$link_value,
    sankey$targets[sankey$link_target + 1L],
    sum
  )
  expect_equal(
    as.vector(by_target[c("c1", "c2")]),
    as.vector(table(complete$cluster))
  )
})

test_that("violin payloads keep the exact summary of every observation", {
  scope <- groups_charts_env()
  set.seed(1)
  values <- c(stats::rexp(6000, 1 / 800), 5, 5, 5, 2, 9)
  table <- data.frame(
    group = factor(
      c(rep("big", 6000), rep("flat", 3), "pair", "pair"),
      levels = c("big", "flat", "pair")
    ),
    nUMI = values
  )
  violin <- scope$cerebroViolinData(
    table,
    "nUMI",
    "group",
    c(big = "#ff0000", flat = "#00ff00", pair = "#0000ff"),
    "Number of transcripts",
    "integer"
  )
  exact <- attr(
    scope$compactViolinData(table, "nUMI", "group"),
    "cerebro_violin_summary"
  )$stats
  expect_identical(as.vector(violin$groups), c("big", "flat", "pair"))
  for (name in c(
    "n",
    "mean",
    "q1",
    "median",
    "q3",
    "lowerfence",
    "upperfence",
    "minimum",
    "maximum",
    "approximate"
  )) {
    expect_equal(as.vector(violin[[name]]), exact[[name]], info = name)
  }
  big <- values[1:6000]
  expect_equal(violin$median[[1L]], stats::median(big))
  expect_equal(violin$mean[[1L]], mean(big))
  expect_true(violin$approximate[[1L]])
  expect_length(violin$violins[[1L]]$density, 96L)
  expect_equal(max(violin$violins[[1L]]$density), 1)
  expect_equal(violin$violins[[1L]]$lo, min(big))
  expect_equal(violin$violins[[1L]]$hi, max(big))
  expect_length(violin$violins[[2L]]$density, 0L)
  expect_length(violin$violins[[3L]]$density, 96L)

  empty <- scope$cerebroViolinData(
    table[0, ],
    "nUMI",
    "group",
    NULL,
    "Number of transcripts",
    "integer"
  )
  expect_length(empty$groups, 0L)
})

test_that("Groups charts are cached per session by cachePlot()", {
  scope <- groups_charts_env()
  eval(
    viewer_r_assignment(viewer_test_path("utility_functions.R"), "cachePlot"),
    scope
  )
  runs <- 0L
  shiny::testServer(
    function(input, output, session) {
      output$chart <- scope$cachePlot(
        scope$renderCerebroChart({
          runs <<- runs + 1L
          list(type = "bar", key = input$key)
        }),
        input$key
      )
    },
    {
      session$setInputs(key = "a")
      expect_identical(output$chart, list(type = "bar", key = "a"))
      session$setInputs(key = "b")
      expect_identical(output$chart$key, "b")
      session$setInputs(key = "a")
      expect_identical(output$chart$key, "a")
      expect_identical(runs, 2L)
    }
  )
})

test_that("Shiny-serialised payloads draw as SVG in the browser renderer", {
  skip_if_no_node()
  scope <- groups_charts_env()
  metadata <- groups_charts_metadata()
  long <- scope$calculateTableAB(
    metadata,
    "sample",
    "cluster",
    "long",
    FALSE
  )
  single <- long[long$sample == "s3", ]
  single$sample <- droplevels(single$sample)
  payloads <- list(
    bar = scope$cerebroBarChartData(long, "sample", "cluster", NULL, FALSE),
    single = scope$cerebroBarChartData(single, "sample", "cluster", NULL, TRUE),
    sankey = scope$cerebroSankeyData(long, "sample", "cluster", NULL, NULL),
    violin = scope$cerebroViolinData(
      data.frame(group = c("a", "a", "a", "b"), v = c(.1, .2, .4, .3)),
      "v",
      "group",
      NULL,
      "Percentage of transcripts",
      "percent"
    )
  )
  ## The JSON Shiny sends: length-one vectors unboxed unless marked I().
  json <- withr::local_tempfile(fileext = ".json")
  writeLines(
    paste0(
      "{",
      paste0(
        '"',
        names(payloads),
        '":',
        vapply(payloads, function(x) shiny:::toJSON(x), character(1)),
        collapse = ","
      ),
      "}"
    ),
    json
  )
  expect_node_script(
    c(
      "const fs = require('fs'), assert = require('assert/strict');",
      "const window = {};",
      sprintf(
        "new Function('window', fs.readFileSync(%s, 'utf8'))(window);",
        encodeString(viewer_test_path("www", "cerebro-charts.js"), quote = "'")
      ),
      sprintf(
        "const p = JSON.parse(fs.readFileSync(%s, 'utf8'));",
        encodeString(json, quote = "'")
      ),
      "const charts = window.CerebroCharts;",
      "const count = (svg, re) => (svg.match(re) || []).length;",
      "let model = charts.normalize(p.bar);",
      "assert.deepEqual(model.values, [[1, 2, 0], [1, 1, 1]]);",
      "let tips = [];",
      "let svg = charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.equal(count(svg, /<rect x=/g), 5);",
      "assert.equal(tips.length, 3);",
      "assert.match(tips[1], /<b>c1:<\\/b> 2/);",
      "svg = charts.chartSvg(model, 600, 400, {1: true}, []);",
      "assert.equal(count(svg, /<rect x=/g), 2);",
      "model = charts.normalize(p.single);",
      "assert.deepEqual(model.categories, ['s3']);",
      "tips = []; charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.match(tips[0], /100\\.0%/);",
      "model = charts.normalize(p.sankey);",
      "assert.equal(model.links.length, 5);",
      "tips = []; svg = charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.equal(count(svg, /class=\"cc-link\"/g), 5);",
      "assert.equal(tips.length, 5 + 3 + 2);",
      "model = charts.normalize(p.violin);",
      "tips = []; svg = charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.equal(count(svg, /<path d=/g), 1);",
      "assert.match(tips[0], /median:<\\/b> 20\\.0%/);",
      "const image = charts.exportSvg({model: charts.normalize(p.bar),",
      "  hidden: {}, plotWidth: 500, plotHeight: 300});",
      "assert.ok(image.width > 500 && image.markup.includes('c2'));",
      "assert.equal(charts.fixed(1234567.4, 0), '1,234,567');",
      "assert.deepEqual(charts.ticks(0, 1, 5).values, [0, 0.2, 0.4, 0.6, 0.8, 1]);",
      "console.log('groups charts render ok');"
    ),
    pass = "groups charts render ok"
  )
})

test_that("count bars, violin panels, lines and scatter payloads draw", {
  skip_if_no_node()
  scope <- groups_charts_env()
  stops <- list(list(0, "#000000"), list(1, "#ffffff"))
  expect_identical(
    scope$cerebroScaleColors(c(0, 0.5, 1), stops),
    c("#000000", "#7F7F7F", "#FFFFFF")
  )
  expect_identical(
    scope$cerebroScaleColors(c(0, 1), stops, reverse = TRUE),
    c("#FFFFFF", "#000000")
  )
  violin <- function(title) {
    scope$cerebroViolinData(
      data.frame(group = c("a", "a", "b", "b"), v = c(1, 2, 3, 5)),
      "v",
      "group",
      c(a = "#ff0000", b = "#0000ff"),
      "Expression level",
      "number",
      title = title,
      fill = "#16a34a"
    )
  }
  set.seed(1)
  n <- 3000L
  payloads <- list(
    count = scope$cerebroCountBarData(
      c("A", "B"),
      c(2, 0),
      c(A = "#ff8000", B = "#777777")
    ),
    panels = scope$cerebroViolinPanels(
      list(violin("G1"), violin("G2"), violin("G3"), violin("G4")),
      "Expression level",
      "number"
    ),
    lines = scope$cerebroLineChartData(
      list(
        list(name = "early", color = "#ff0000", x = 1:5, y = c(0, 1, 2, 1, 0)),
        list(name = "late", color = "#0000ff", x = 3:7, y = c(0, 2, 4, 2, 0))
      ),
      "Pseudotime",
      "Density",
      fill_opacity = 0.3,
      legend_title = "state"
    ),
    scatter = scope$cerebroScatterData(
      x = stats::runif(n),
      y = stats::runif(n) * 100,
      groups = factor(rep(c("early", "late"), length.out = n)),
      colors = c(early = "#ff0000", late = "#0000ff"),
      x_title = "Pseudotime",
      y_title = "nUMI",
      group_title = "State",
      hover = list(Transcripts = seq_len(n), Cell = paste0("c", seq_len(n))),
      trend = list(x = c(0, 1), y = c(10, 90), color = "#f97316", width = 2)
    )
  )
  expect_identical(
    as.character(payloads$panels$panels[[1L]]$colors),
    c(
      "#16a34a",
      "#16a34a"
    )
  )
  json <- withr::local_tempfile(fileext = ".json")
  writeLines(
    paste0(
      "{",
      paste0(
        '"',
        names(payloads),
        '":',
        vapply(payloads, function(x) shiny:::toJSON(x), character(1)),
        collapse = ","
      ),
      "}"
    ),
    json
  )
  expect_node_script(
    c(
      "const fs = require('fs'), assert = require('assert/strict');",
      "const window = {atob: (s) => Buffer.from(s, 'base64').toString('binary')};",
      sprintf(
        "new Function('window', fs.readFileSync(%s, 'utf8'))(window);",
        encodeString(viewer_test_path("www", "cerebro-charts.js"), quote = "'")
      ),
      sprintf(
        "const p = JSON.parse(fs.readFileSync(%s, 'utf8'));",
        encodeString(json, quote = "'")
      ),
      "const charts = window.CerebroCharts;",
      "const count = (svg, re) => (svg.match(re) || []).length;",
      "let model = charts.normalize(p.count);",
      "assert.deepEqual(model.categoryColors, ['#ff8000', '#777777']);",
      "let tips = [], svg = charts.chartSvg(model, 600, 400, {}, tips);",
      "assert.equal(count(svg, /<rect x=/g), 1);",
      "assert.match(tips[0], /<b>A:<\\/b> 2/);",
      "model = charts.normalize(p.panels);",
      "assert.equal(model.panels.length, 4);",
      "assert.equal(model.count, 8);",
      "tips = []; svg = charts.chartSvg(model, 900, 600, {}, tips);",
      "assert.equal(count(svg, /fill-opacity=\"0.5\"/g), 8);",
      "assert.match(svg, />G4</);",
      "assert.match(tips[0], /a · G1/);",
      "model = charts.normalize(p.lines);",
      "let extras = {}; svg = charts.chartSvg(model, 600, 400, {}, [], false, extras);",
      "assert.equal(count(svg, /fill-opacity=\"0.3\"/g), 2);",
      "assert.match(extras.hover(300, 200), /early/);",
      "svg = charts.chartSvg(model, 600, 400, {0: true}, [], false, {});",
      "assert.equal(count(svg, /fill-opacity=\"0.3\"/g), 1);",
      "model = charts.normalize(p.scatter);",
      sprintf("assert.equal(model.n, %d);", n),
      "assert.equal(model.x.length, model.n);",
      "assert.equal(model.order[0][1] - model.order[0][0], model.n / 2);",
      "extras = {}; svg = charts.chartSvg(model, 600, 400, {}, [], false, extras);",
      "assert.ok(extras.canvas && typeof extras.canvas.paint === 'function');",
      "assert.match(svg, /stroke=\"#f97316\"/);",
      "assert.match(svg, /cc-points/);",
      "console.log('cerebro charts render ok');"
    ),
    pass = "cerebro charts render ok"
  )
})

test_that("a violin group named with an empty string keeps its shape", {
  scope <- groups_charts_env()
  table <- data.frame(
    group = factor(c(rep("", 20), rep("a", 20)), levels = c("", "a")),
    value = c(seq(1, 20), seq(5, 24))
  )
  violin <- scope$cerebroViolinData(
    table,
    "value",
    "group",
    c(a = "#ff0000"),
    "Value",
    "number"
  )
  expect_identical(as.vector(violin$groups), c("", "a"))
  expect_gt(length(violin$violins[[1L]]$y %||% violin$violins[[1L]][[1L]]), 0L)
  expect_identical(
    lengths(violin$violins[[1L]]),
    lengths(violin$violins[[2L]])
  )
})
