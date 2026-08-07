/* Cerebro Dataset Builder: semantic client interaction and accessibility. */
(function () {
  "use strict";

  var narrowManager = window.matchMedia("(max-width: 42.5rem)");
  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  var lastFileDialogTrigger = null;
  var activeFileDialog = null;
  var statusTimer = null;
  var lastAnnouncement = "";
  var observedPrimaryAction = null;

  function send(name, value) {
    if (window.Shiny) {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  function focusableElements(root) {
    return Array.from(
      root.querySelectorAll(
        "button:not([disabled]), [href], input:not([disabled]), " +
          "select:not([disabled]), textarea:not([disabled]), " +
          '[tabindex]:not([tabindex="-1"])'
      )
    ).filter(function (element) {
      return element.getClientRects().length > 0;
    });
  }

  function restoreFocus(dialog) {
    var target = dialog && dialog.__builderRestoreFocus;
    window.setTimeout(function () {
      if (target && document.contains(target)) target.focus();
    }, 0);
  }

  function trapDialogKeydown(event) {
    var dialog = event.currentTarget;
    if (event.key === "Escape") {
      event.preventDefault();
      event.stopPropagation();
      if (dialog.__builderClose) dialog.__builderClose();
      return;
    }
    if (event.key !== "Tab") return;
    var items = focusableElements(dialog);
    if (!items.length) {
      event.preventDefault();
      dialog.focus();
      return;
    }
    var first = items[0];
    var last = items[items.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function prepareDialog(dialog, trigger, close) {
    dialog.setAttribute("role", "dialog");
    dialog.setAttribute("aria-modal", "true");
    dialog.setAttribute("tabindex", "-1");
    dialog.__builderRestoreFocus = trigger || document.activeElement;
    dialog.__builderClose = close;
    dialog.addEventListener("keydown", trapDialogKeydown);
    document.body.classList.add("builder-dialog-open");
    window.setTimeout(function () {
      var items = focusableElements(dialog);
      (items[0] || dialog).focus();
    }, 0);
  }

  function updateDialogLock() {
    document.body.classList.toggle(
      "builder-dialog-open",
      document.querySelector('[aria-modal="true"]') !== null
    );
  }

  function closeFileDialog(sheet) {
    send("close_browser", Date.now());
    var trigger = sheet.__builderRestoreFocus;
    window.setTimeout(function () {
      updateDialogLock();
      if (trigger && document.contains(trigger)) trigger.focus();
    }, 50);
  }

  function enhanceFileDialog(sheet) {
    if (!sheet || sheet.dataset.builderDialog === "true") return;
    sheet.dataset.builderDialog = "true";
    activeFileDialog = sheet;
    var title = sheet.querySelector(".sheet-title");
    if (title) {
      title.id = title.id || "builder-file-dialog-title";
      sheet.setAttribute("aria-labelledby", title.id);
    } else {
      sheet.setAttribute("aria-label", "Choose datasets");
    }
    prepareDialog(sheet, lastFileDialogTrigger, function () {
      closeFileDialog(sheet);
    });
  }

  function setRailDesktopSemantics(rail) {
    rail.setAttribute("role", "complementary");
    rail.setAttribute("aria-label", "Datasets");
    rail.removeAttribute("aria-modal");
    rail.removeAttribute("aria-hidden");
  }

  function updateRailSummary() {
    var summary = document.querySelector(".rail-summary");
    var rail = document.querySelector(".rail");
    if (!summary || !rail) return;
    var current = rail.querySelector(".ds.is-active");
    var name = current && current.querySelector(".nm");
    var readiness = current && current.querySelector(".rail-readiness");
    var issues = current && current.querySelector(".rail-issues");
    var nextName = name ? name.textContent.trim() : "No dataset selected";
    var nextState = [
      readiness ? readiness.textContent.trim() : "",
      issues ? issues.textContent.trim() : "",
    ]
      .filter(Boolean)
      .join(" · ");
    var nameOutput = summary.querySelector(".rail-summary-name");
    var stateOutput = summary.querySelector(".rail-summary-state");
    if (nameOutput.textContent !== nextName) nameOutput.textContent = nextName;
    if (stateOutput.textContent !== nextState) stateOutput.textContent = nextState;
  }

  function closeDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !rail.classList.contains("is-manager-open")) return;
    rail.classList.remove("is-manager-open");
    if (backdrop) backdrop.classList.remove("is-open");
    if (summary) summary.setAttribute("aria-expanded", "false");
    rail.removeEventListener("keydown", trapDialogKeydown);
    setRailDesktopSemantics(rail);
    if (narrowManager.matches) rail.setAttribute("aria-hidden", "true");
    updateDialogLock();
    restoreFocus(rail);
  }

  function openDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !summary || !narrowManager.matches) return;
    rail.classList.add("is-manager-open");
    if (backdrop) backdrop.classList.add("is-open");
    rail.removeAttribute("aria-hidden");
    summary.setAttribute("aria-expanded", "true");
    prepareDialog(rail, summary, closeDatasetManager);
  }

  function applyRailMode() {
    var rail = document.querySelector(".rail");
    if (!rail) return;
    if (narrowManager.matches) {
      if (!rail.classList.contains("is-manager-open")) {
        rail.setAttribute("aria-hidden", "true");
      }
    } else {
      rail.classList.remove("is-manager-open");
      var backdrop = document.querySelector(".rail-manager-backdrop");
      if (backdrop) backdrop.classList.remove("is-open");
      var summary = document.querySelector(".rail-summary");
      if (summary) summary.setAttribute("aria-expanded", "false");
      rail.removeEventListener("keydown", trapDialogKeydown);
      setRailDesktopSemantics(rail);
      updateDialogLock();
    }
  }

  function setupRail() {
    var rail = document.querySelector(".rail");
    if (!rail || rail.dataset.builderManager === "true") return;
    rail.dataset.builderManager = "true";
    rail.id = rail.id || "builder-dataset-manager";
    setRailDesktopSemantics(rail);

    var summary = document.createElement("button");
    summary.type = "button";
    summary.className = "rail-summary";
    summary.setAttribute("aria-controls", rail.id);
    summary.setAttribute("aria-expanded", "false");
    var text = document.createElement("span");
    text.className = "rail-summary-copy";
    var name = document.createElement("span");
    name.className = "rail-summary-name";
    var state = document.createElement("span");
    state.className = "rail-summary-state";
    text.appendChild(name);
    text.appendChild(state);
    var affordance = document.createElement("span");
    affordance.className = "rail-summary-action";
    affordance.textContent = "Dataset Manager";
    summary.appendChild(text);
    summary.appendChild(affordance);
    rail.parentNode.insertBefore(summary, rail);

    var backdrop = document.createElement("button");
    backdrop.type = "button";
    backdrop.className = "rail-manager-backdrop";
    backdrop.setAttribute("aria-label", "Close Dataset Manager");
    rail.parentNode.insertBefore(backdrop, rail);

    var close = document.createElement("button");
    close.type = "button";
    close.className = "rail-manager-close";
    close.setAttribute("aria-label", "Close Dataset Manager");
    close.textContent = "Close";
    rail.querySelector(".rail-head").appendChild(close);
    updateRailSummary();
    applyRailMode();
  }

  function showRemoveConfirmation(removeDataset) {
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-confirm-dialog";
    var title = document.createElement("h2");
    title.id = "builder-remove-title";
    title.textContent = "Remove dataset setup?";
    var message = document.createElement("p");
    message.textContent = "You can undo the most recent removal in this session.";
    var actions = document.createElement("div");
    actions.className = "builder-confirm-actions";
    var cancel = document.createElement("button");
    cancel.type = "button";
    cancel.className = "btn";
    cancel.textContent = "Keep dataset";
    var confirm = document.createElement("button");
    confirm.type = "button";
    confirm.className = "btn btn-quiet";
    confirm.textContent = "Remove dataset";
    actions.appendChild(cancel);
    actions.appendChild(confirm);
    dialog.appendChild(title);
    dialog.appendChild(message);
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);

    function close() {
      backdrop.remove();
      updateDialogLock();
      restoreFocus(dialog);
    }
    cancel.addEventListener("click", close);
    confirm.addEventListener("click", function () {
      send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      close();
    });
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, removeDataset, close);
  }

  function setCurrentStage(stage) {
    document.querySelectorAll(".builder-stage").forEach(function (candidate) {
      candidate.removeAttribute("aria-current");
    });
    if (stage) stage.setAttribute("aria-current", "stage");
  }

  var stageObserver = new IntersectionObserver(
    function (entries) {
      var visible = entries
        .filter(function (entry) {
          return entry.isIntersecting;
        })
        .sort(function (left, right) {
          return right.intersectionRatio - left.intersectionRatio;
        });
      if (visible.length) setCurrentStage(visible[0].target);
    },
    { rootMargin: "-15% 0px -55% 0px", threshold: [0.15, 0.5, 0.85] }
  );

  function registerStages() {
    var stages = document.querySelectorAll(".builder-stage");
    stages.forEach(function (stage) {
      if (stage.dataset.builderStage === "true") return;
      stage.dataset.builderStage = "true";
      stage.setAttribute("role", "region");
      var heading = stage.querySelector("h2");
      if (heading) {
        heading.id = heading.id || stage.id + "-heading";
        stage.setAttribute("aria-labelledby", heading.id);
      }
      stage.addEventListener("focusin", function () {
        setCurrentStage(stage);
      });
      stageObserver.observe(stage);
    });
    if (stages.length && !document.querySelector('[aria-current="stage"]')) {
      setCurrentStage(stages[0]);
    }
  }

  function ensureLiveRegion() {
    var live = document.getElementById("builder-live-status");
    if (live) return live;
    live = document.createElement("div");
    live.id = "builder-live-status";
    live.className = "visually-hidden";
    live.setAttribute("role", "status");
    live.setAttribute("aria-live", "polite");
    live.setAttribute("aria-atomic", "true");
    document.body.appendChild(live);
    return live;
  }

  function scheduleStatusAnnouncement(text) {
    var next = String(text || "").replace(/\s+/g, " ").trim();
    if (!next || next === lastAnnouncement) return;
    window.clearTimeout(statusTimer);
    statusTimer = window.setTimeout(function () {
      ensureLiveRegion().textContent = next;
      lastAnnouncement = next;
    }, 350);
  }

  function updateStatusSemantics() {
    ["busy", "result_card", "review_action_summary"].forEach(function (id) {
      var output = document.getElementById(id);
      if (!output) return;
      output.setAttribute("role", "status");
      output.setAttribute("aria-live", "polite");
      output.setAttribute("aria-atomic", "true");
    });
    var status = ["#busy", "#result_card", "#review_action_summary"]
      .map(function (selector) {
        var node = document.querySelector(selector);
        return node ? node.textContent : "";
      })
      .filter(Boolean)
      .join(". ");
    scheduleStatusAnnouncement(status);
  }

  var primaryObserver = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.target === observedPrimaryAction) {
          window.__builderPrimaryActionVisible =
            entry.isIntersecting && entry.intersectionRatio >= 0.99;
        }
      });
    },
    { threshold: [0, 0.99, 1] }
  );

  function registerPrimaryAction() {
    var action = document.getElementById("build");
    if (action === observedPrimaryAction) return;
    if (observedPrimaryAction) primaryObserver.unobserve(observedPrimaryAction);
    observedPrimaryAction = action;
    window.__builderPrimaryActionVisible = false;
    if (action) primaryObserver.observe(action);
  }

  function updateMotionDuration() {
    if (reducedMotion.matches) {
      window.__builderMotionDuration = 0;
      return;
    }
    var duration = getComputedStyle(document.documentElement)
      .getPropertyValue("--dur")
      .trim();
    window.__builderMotionDuration = Math.round(parseFloat(duration) * 1000);
  }

  function plotSummaryRows(plot) {
    return (plot.data || []).map(function (trace, index) {
      var points = Math.max(
        Array.isArray(trace.x) ? trace.x.length : 0,
        Array.isArray(trace.y) ? trace.y.length : 0
      );
      return {
        name: trace.name || "Series " + (index + 1),
        points: points,
      };
    });
  }

  function renderPlotSummary(plot) {
    if (!plot.data) return;
    var rows = plotSummaryRows(plot);
    var summary = plot.parentNode.querySelector(".builder-preview-summary");
    if (!summary) {
      summary = document.createElement("details");
      summary.className = "builder-preview-summary";
      summary.id = (plot.id || "builder-preview") + "-summary";
      var toggle = document.createElement("summary");
      toggle.textContent = "Accessible preview data";
      var table = document.createElement("table");
      table.innerHTML =
        "<thead><tr><th scope='col'>Series</th>" +
        "<th scope='col'>Points</th></tr></thead><tbody></tbody>";
      summary.appendChild(toggle);
      summary.appendChild(table);
      plot.insertAdjacentElement("afterend", summary);
    }
    var body = summary.querySelector("tbody");
    body.textContent = "";
    rows.forEach(function (row) {
      var tr = document.createElement("tr");
      var name = document.createElement("th");
      name.scope = "row";
      name.textContent = row.name;
      var points = document.createElement("td");
      points.textContent = String(row.points);
      tr.appendChild(name);
      tr.appendChild(points);
      body.appendChild(tr);
    });
    var total = rows.reduce(function (sum, row) {
      return sum + row.points;
    }, 0);
    plot.setAttribute("role", "img");
    plot.setAttribute(
      "aria-label",
      "Spatial alignment preview with " + rows.length + " series and " +
        total + " plotted points."
    );
    plot.setAttribute("aria-describedby", summary.id);
  }

  function enhancePlot(plot) {
    if (!plot || plot.dataset.builderPreview === "true") return;
    if (!plot.data || typeof plot.on !== "function") return;
    plot.dataset.builderPreview = "true";
    plot.on("plotly_afterplot", function () {
      renderPlotSummary(plot);
      plot.classList.remove("is-updating");
    });
    plot.on("plotly_relayouting", function () {
      plot.classList.add("is-updating");
    });
    renderPlotSummary(plot);
  }

  function colourLabel(input) {
    var swatch = input.closest(".swatch");
    var name = swatch && swatch.querySelector(".swatch-name");
    var level = name ? name.textContent.trim() : input.dataset.level || "Colour";
    return level + ": " + input.value.toUpperCase();
  }

  function enhanceColour(input) {
    input.setAttribute("aria-label", colourLabel(input));
  }

  function enhanceDynamicContent() {
    setupRail();
    updateRailSummary();
    registerStages();
    registerPrimaryAction();
    updateStatusSemantics();
    var sheet = document.querySelector(".sheet");
    if (sheet) {
      enhanceFileDialog(sheet);
    } else if (activeFileDialog) {
      var closedFileDialog = activeFileDialog;
      activeFileDialog = null;
      updateDialogLock();
      restoreFocus(closedFileDialog);
    }
    document.querySelectorAll(".js-plotly-plot").forEach(enhancePlot);
    document.querySelectorAll('input[type="color"]').forEach(enhanceColour);
  }

  document.addEventListener("click", function (event) {
    var target = event.target;
    var openBrowser = target.closest("#open_browser");
    if (openBrowser) lastFileDialogTrigger = openBrowser;

    var fileClose = target.closest(".sheet [onclick*='close_browser']");
    if (fileClose) {
      event.preventDefault();
      event.stopPropagation();
      closeFileDialog(fileClose.closest(".sheet"));
      return;
    }
    if (target.classList.contains("sheet-backdrop")) {
      closeFileDialog(target.querySelector(".sheet"));
      return;
    }

    var managerSummary = target.closest(".rail-summary");
    if (managerSummary) {
      openDatasetManager();
      return;
    }
    if (
      target.closest(".rail-manager-close") ||
      target.closest(".rail-manager-backdrop")
    ) {
      closeDatasetManager();
      return;
    }

    var removeDataset = target.closest(".builder-drop");
    if (removeDataset) {
      event.preventDefault();
      event.stopPropagation();
      if (removeDataset.dataset.confirm === "true") {
        showRemoveConfirmation(removeDataset);
      } else {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      return;
    }

    var duplicateDataset = target.closest(".builder-duplicate");
    if (duplicateDataset) {
      send("duplicate_ds", duplicateDataset.dataset.ds);
      return;
    }
    var initialDataset = target.closest(".builder-select-initial");
    if (initialDataset) {
      send("select_initial", initialDataset.dataset.ds);
      return;
    }
    var reorderDataset = target.closest(".builder-reorder");
    if (reorderDataset) {
      send("reorder_ds", {
        id: reorderDataset.dataset.ds,
        direction: reorderDataset.dataset.direction,
      });
      return;
    }
    var pick = target.closest(".builder-pick");
    if (pick) {
      send("pick", pick.dataset.ds);
      if (narrowManager.matches) closeDatasetManager();
      return;
    }
    var browse = target.closest(".builder-browse");
    if (browse) {
      send("browse_to", browse.dataset.path);
      return;
    }
    var choose = target.closest(".builder-choose");
    if (choose) {
      event.preventDefault();
      choose.classList.toggle("is-selected");
      choose.setAttribute(
        "aria-pressed",
        choose.classList.contains("is-selected") ? "true" : "false"
      );
      var addFiles = document.querySelector(".builder-add-files");
      if (addFiles) {
        addFiles.disabled = !document.querySelector(".builder-choose.is-selected");
      }
      return;
    }
    var addFiles = target.closest(".builder-add-files");
    if (addFiles) {
      var paths = Array.from(
        document.querySelectorAll(".builder-choose.is-selected")
      ).map(function (file) {
        return file.dataset.path;
      });
      if (paths.length) send("choose_files", paths);
      return;
    }
    var example = target.closest(".example-btn");
    if (example) {
      send("use_example", example.dataset.ex);
      closeFileDialog(example.closest(".sheet"));
    }
  });

  document.addEventListener("keydown", function (event) {
    var pick = event.target.closest(".builder-pick");
    if (!pick || !event.altKey) return;
    var direction = null;
    if (event.key === "ArrowUp") direction = "up";
    if (event.key === "ArrowDown") direction = "down";
    if (!direction) return;
    event.preventDefault();
    send("reorder_ds", { id: pick.dataset.ds, direction: direction });
  });

  document.addEventListener("input", function (event) {
    if (event.target.matches('input[type="color"]')) {
      enhanceColour(event.target);
    }
  });

  document.addEventListener("shiny:connected", enhanceDynamicContent);
  document.addEventListener("shiny:sessioninitialized", function () {
    window.Shiny.addCustomMessageHandler(
      "builder_used_examples",
      function (message) {
        var used = new Set((message && message.ids) || []);
        document.querySelectorAll(".example-btn[data-ex]").forEach(function (el) {
          var taken = used.has(el.dataset.ex);
          el.classList.toggle("is-taken", taken);
          el.disabled = taken;
          el.setAttribute("aria-disabled", taken ? "true" : "false");
        });
      }
    );
  });

  new MutationObserver(enhanceDynamicContent).observe(document.documentElement, {
    childList: true,
    subtree: true,
    characterData: true,
  });

  if (narrowManager.addEventListener) {
    narrowManager.addEventListener("change", applyRailMode);
    reducedMotion.addEventListener("change", updateMotionDuration);
  } else {
    narrowManager.addListener(applyRailMode);
    reducedMotion.addListener(updateMotionDuration);
  }
  updateMotionDuration();
  ensureLiveRegion();
  enhanceDynamicContent();
})();
