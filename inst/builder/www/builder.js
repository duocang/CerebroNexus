/* Cerebro Dataset Builder: semantic client interaction and accessibility. */
(function () {
  "use strict";

  var narrowManager = window.matchMedia("(max-width: 43.75rem)");
  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  var statusTimer = null;
  var lastAnnouncement = "";
  var observedPrimaryAction = null;
  var firstRunKey = "cerebro-builder-first-run-v1";
  var exampleMessageHandlerRegistered = false;
  var buildDialogHandlerRegistered = false;
  var clientUploadSequence = 0;

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

  function isTextInput(target) {
    return Boolean(target && target.closest(
      "input, textarea, select, [contenteditable='true'], .selectize-input"
    ));
  }

  function setupFirstRun() {
    var guide = document.querySelector(".builder-first-run[data-first-run]");
    if (!guide || guide.dataset.ready === "true") return;
    guide.dataset.ready = "true";
    var dismissed = false;
    try { dismissed = window.localStorage.getItem(firstRunKey) === "dismissed"; } catch (error) {}
    if (dismissed) guide.hidden = true;
  }

  function clientLoadingStage(label, status) {
    var section = document.createElement("section");
    section.className = "builder-stage builder-loading-stage is-client";
    section.setAttribute("aria-live", "polite");
    section.setAttribute("aria-atomic", "true");
    var copy = document.createElement("div");
    copy.className = "builder-loading-copy";
    var kicker = document.createElement("span");
    kicker.className = "builder-loading-kicker";
    kicker.textContent = "Dataset import";
    var title = document.createElement("h2");
    title.textContent = "Loading dataset";
    var name = document.createElement("p");
    name.className = "builder-loading-name";
    name.textContent = label;
    var state = document.createElement("p");
    state.className = "builder-loading-status";
    state.textContent = status;
    copy.appendChild(kicker);
    copy.appendChild(title);
    copy.appendChild(name);
    copy.appendChild(state);
    var progress = document.createElement("div");
    progress.className = "builder-loading-progress";
    progress.setAttribute("role", "progressbar");
    progress.setAttribute("aria-label", status);
    progress.setAttribute("aria-valuetext", status);
    progress.appendChild(document.createElement("span"));
    section.appendChild(copy);
    section.appendChild(progress);
    return section;
  }

  function showClientLoadingWorkbench(label, status) {
    var workbench = document.getElementById("workbench");
    var list = document.getElementById("ds_list");
    if (!workbench || !list) return;
    var hasReadyDataset = list.querySelector(".ds:not(.ds--import)");
    if (hasReadyDataset && !workbench.querySelector(".builder-empty-state")) return;
    workbench.replaceChildren(clientLoadingStage(label, status));
  }

  function beginClientDatasetUpload(input) {
    var files = Array.from(input.files || []);
    var list = document.getElementById("ds_list");
    var importList = document.getElementById("ds_import_list");
    if (!files.length || !list || !importList) return;
    var empty = list.querySelector(".rail-empty");
    if (empty) empty.remove();
    files.forEach(function (file, index) {
      clientUploadSequence += 1;
      var label = file && file.name ? file.name : "Selected dataset";
      var row = document.createElement("div");
      row.className = "ds ds--import ds--client-upload";
      row.dataset.clientUpload = String(clientUploadSequence);
      row.setAttribute("role", "status");
      row.setAttribute("aria-label", label + ". Uploading.");
      var body = document.createElement("span");
      body.className = "ds-body";
      var name = document.createElement("span");
      name.className = "nm";
      name.textContent = label;
      var status = document.createElement("span");
      status.className = "builder-import-status is-reading";
      status.textContent = "Uploading…";
      body.appendChild(name);
      body.appendChild(status);
      row.appendChild(body);
      importList.appendChild(row);
      if (index === 0) {
        showClientLoadingWorkbench(label, "Uploading the selected file…");
      }
    });
    scheduleStatusAnnouncement(
      files.length === 1 ? "Uploading selected dataset." :
        "Uploading " + files.length + " selected datasets."
    );
  }

  function updateDialogLock() {
    document.body.classList.toggle(
      "builder-dialog-open",
      document.querySelector('[aria-modal="true"]:not([hidden])') !== null
    );
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
    var reviewStatus = current && current.querySelector(".rail-review-status");
    var nextName = name ? name.textContent.trim() : "No dataset selected";
    var nextState = [
      reviewStatus ? reviewStatus.textContent.trim() : "",
    ]
      .filter(Boolean)
      .join(" · ");
    var nameOutput = summary.querySelector(".rail-summary-name");
    var stateOutput = summary.querySelector(".rail-summary-state");
    if (nameOutput.textContent !== nextName) nameOutput.textContent = nextName;
    if (stateOutput.textContent !== nextState) stateOutput.textContent = nextState;
    if (nextState) scheduleStatusAnnouncement(nextName + ". " + nextState + ".");
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
    dialog.className = "builder-dialog builder-confirm-dialog";
    var title = document.createElement("h2");
    title.id = "builder-remove-title";
    title.textContent = "Remove dataset setup?";
    var message = document.createElement("p");
    message.textContent = "You can undo the most recent removal in this session.";
    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions";
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

  function showAnalysisInfo(infoButton) {
    if (document.querySelector(".builder-analysis-info-backdrop")) return;

    var backdrop = document.createElement("div");
    backdrop.className = "builder-analysis-info-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-analysis-info-dialog";
    var header = document.createElement("div");
    header.className = "builder-analysis-info-header";
    var title = document.createElement("h2");
    title.id = "builder-analysis-info-title";
    title.textContent = infoButton.dataset.title || "Analysis details";
    var closeButton = document.createElement("button");
    closeButton.type = "button";
    closeButton.className = "builder-analysis-info-close";
    closeButton.setAttribute("aria-label", "Close analysis information");
    closeButton.textContent = "Close";
    header.appendChild(title);
    header.appendChild(closeButton);

    var description = document.createElement("p");
    description.id = "builder-analysis-info-description";
    description.className = "builder-analysis-info-description";
    description.textContent = infoButton.dataset.description || "";

    var facts = document.createElement("dl");
    facts.className = "builder-analysis-info-facts";
    [
      { label: "Available in", value: infoButton.dataset.pages },
      { label: "Typical time", value: infoButton.dataset.cost },
      { label: "Requires", value: infoButton.dataset.prerequisite },
      { label: "Network", value: infoButton.dataset.network },
      {
        label: "If already present",
        value: infoButton.dataset.replacement,
        wide: true,
      },
      { label: "If skipped", value: infoButton.dataset.skip, wide: true },
    ].forEach(function (fact) {
      var item = document.createElement("div");
      item.className = "builder-analysis-info-fact" +
        (fact.wide ? " is-wide" : "");
      var label = document.createElement("dt");
      label.textContent = fact.label;
      var value = document.createElement("dd");
      value.textContent = fact.value || "Not specified.";
      item.appendChild(label);
      item.appendChild(value);
      facts.appendChild(item);
    });

    dialog.appendChild(header);
    dialog.appendChild(description);
    dialog.appendChild(facts);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    dialog.setAttribute("aria-describedby", description.id);

    function close() {
      backdrop.remove();
      updateDialogLock();
      restoreFocus(dialog);
    }
    closeButton.addEventListener("click", close);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, infoButton, close);
  }

  function showBuildDialog(message) {
    if (document.querySelector(".builder-build-dialog-backdrop")) return;
    var trigger = document.getElementById("build");
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop builder-build-dialog-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog builder-build-dialog";
    var title = document.createElement("h2");
    title.id = "builder-build-dialog-title";
    var defaultTitles = {
      conflict: "Files already exist",
      unreviewed: "Some datasets have not been reviewed",
      needs_attention: "Some datasets still need attention",
      datasets: "Ready to build all datasets?",
    };
    title.textContent = message.title || defaultTitles[message.type] ||
      "Ready to build all datasets?";
    dialog.appendChild(title);

    var description = document.createElement("p");
    if (message.type === "conflict") {
      description.textContent = "Some outputs already exist in this folder:";
    } else if (message.type === "unreviewed") {
      description.textContent = "Review every dataset before building.";
    } else if (message.type === "needs_attention") {
      description.textContent = "Resolve the highlighted issues before building.";
    } else {
      description.textContent = "All " + message.count +
        " datasets have been reviewed.";
    }
    dialog.appendChild(description);

    var list = document.createElement(message.type === "datasets" ? "ol" : "ul");
    list.className = "builder-build-dialog-list";
    var values = message.type === "conflict" ? message.files : message.names;
    var shown = values.slice(0, 4);
    shown.forEach(function (name) {
      var item = document.createElement("li");
      item.textContent = name;
      list.appendChild(item);
    });
    if (values.length > shown.length) {
      var more = document.createElement("li");
      more.textContent = "…and " + (values.length - shown.length) + " more";
      list.appendChild(more);
    }
    dialog.appendChild(list);
    if (message.type === "conflict") {
      var question = document.createElement("p");
      question.textContent = "What would you like to do?";
      dialog.appendChild(question);
    }

    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions builder-build-dialog-actions";
    var buttons;
    if (message.type === "conflict") {
      buttons = [
        { label: "Cancel", action: "cancel", className: "btn" },
        { label: "Replace existing files", action: "replace", className: "btn btn-replace" },
        { label: "Choose another folder", action: "choose_another", className: "btn btn-action" },
      ];
    } else if (message.type === "unreviewed") {
      buttons = [
        { label: "Cancel", action: "cancel", className: "btn" },
        { label: "Review now", action: "review_now", className: "btn btn-action" },
      ];
    } else if (message.type === "needs_attention") {
      buttons = [
        { label: "Cancel", action: "cancel", className: "btn" },
        { label: "Fix issues", action: "fix_issues", className: "btn btn-action" },
      ];
    } else {
      buttons = [
        { label: "Back to review", action: "cancel", className: "btn" },
        { label: "Continue", action: "continue", className: "btn btn-action" },
      ];
    }
    var closed = false;
    function close(action) {
      if (closed) return;
      closed = true;
      backdrop.remove();
      updateDialogLock();
      restoreFocus(dialog);
      send("builder_build_dialog", {
        action: action || "cancel",
        nonce: Date.now(),
      });
    }
    buttons.forEach(function (definition) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = definition.className;
      button.textContent = definition.label;
      button.addEventListener("click", function () { close(definition.action); });
      actions.appendChild(button);
    });
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close("cancel");
    });
    prepareDialog(dialog, trigger, close);
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

  function updatePipelines() {
    var order = ["queued", "building", "complete"];
    document.querySelectorAll(".pipeline[data-pipeline-state]").forEach(function (pipeline) {
      var state = pipeline.dataset.pipelineState;
      var activeIndex = order.indexOf(state);
      pipeline.querySelectorAll(".pipeline-step").forEach(function (step) {
        var index = order.indexOf(step.dataset.step);
        var current = step.dataset.step === state;
        var complete = state !== "failure" && index >= 0 && index < activeIndex;
        step.classList.toggle("is-complete", complete);
        step.classList.toggle("is-current", current);
        if (current) {
          step.setAttribute("aria-current", "step");
        } else {
          step.removeAttribute("aria-current");
        }
      });
    });
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
    if (!input.hasAttribute("aria-label")) {
      input.setAttribute("aria-label", colourLabel(input));
    }
  }

  function updateGroupColor(input) {
    var item = input.closest(".group-color-item");
    var editor = input.closest(".builder-group-colors");
    var normalized = input.value.toUpperCase();
    var hex = item && item.querySelector(".group-color-hex");
    var status = editor && editor.querySelector(".group-color-status");
    var label = item && item.querySelector(".group-color-name");
    if (hex) hex.textContent = normalized;
    if (status) {
      status.textContent = "Color for " +
        (label ? label.textContent.trim() : input.dataset.level) +
        " changed to " + normalized + ".";
    }
    send(input.dataset.inputId, {
      group: input.dataset.group,
      level: input.dataset.level,
      color: normalized,
      nonce: Date.now(),
    });
  }

  function filterGroupColors(search) {
    var editor = search.closest(".builder-group-colors");
    if (!editor) return;
    var grid = editor.querySelector(".group-color-grid");
    var query = search.value.trim().toLowerCase();
    grid.classList.toggle("is-searching", query.length > 0);
    grid.querySelectorAll(".group-color-item").forEach(function (item) {
      item.hidden = query.length > 0 &&
        !item.dataset.search.includes(query);
    });
  }

  function toggleGroupColors(button) {
    var editor = button.closest(".builder-group-colors");
    if (!editor) return;
    var grid = editor.querySelector(".group-color-grid");
    var showAll = button.dataset.action === "show-all";
    grid.classList.toggle("is-expanded", showAll);
    grid.classList.toggle("is-collapsed", !showAll);
    editor.querySelector('[data-action="show-all"]').hidden = showAll;
    editor.querySelector('[data-action="show-fewer"]').hidden = !showAll;
  }

  function enhanceDynamicContent() {
    if (window.BuilderIcons) window.BuilderIcons.decorate(document);
    setupRail();
    updateRailSummary();
    registerStages();
    registerPrimaryAction();
    updateStatusSemantics();
    updatePipelines();
    setupFirstRun();
    if (document.querySelector(".result-card.success")) {
      var guide = document.querySelector(".builder-first-run");
      if (guide) guide.hidden = true;
      try { window.localStorage.setItem(firstRunKey, "dismissed"); } catch (error) {}
    }
    updateDialogLock();
    document.querySelectorAll(".js-plotly-plot").forEach(enhancePlot);
    document.querySelectorAll('input[type="color"]').forEach(enhanceColour);
  }

  document.addEventListener("click", function (event) {
    var target = event.target;
    var groupColorToggle = target.closest(".group-color-toggle");
    if (groupColorToggle) {
      event.preventDefault();
      toggleGroupColors(groupColorToggle);
      return;
    }
    var removeTable = target.closest(".enhance-table-remove");
    if (removeTable) {
      event.preventDefault();
      send("enhance-table_action", {
        action: "remove",
        key: removeTable.dataset.tableKey,
        nonce: Date.now(),
      });
      return;
    }
    var infoButton = target.closest(".enhance-info-button");
    if (infoButton) {
      event.preventDefault();
      event.stopPropagation();
      showAnalysisInfo(infoButton);
      return;
    }
    var dismissGuide = target.closest(".builder-first-run-dismiss");
    if (dismissGuide) {
      dismissGuide.closest(".builder-first-run").hidden = true;
      try { window.localStorage.setItem(firstRunKey, "dismissed"); } catch (error) {}
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

    var removePendingUpload = target.closest(".pending-upload-remove");
    if (removePendingUpload) {
      event.preventDefault();
      event.stopPropagation();
      send("cancel_pending_upload", {
        id: removePendingUpload.dataset.uploadId,
        nonce: Date.now(),
      });
      return;
    }

    var pickImport = target.closest(".builder-pick-import");
    if (pickImport) {
      send("pick_import", {
        id: pickImport.dataset.importId,
        nonce: Date.now(),
      });
      if (narrowManager.matches) closeDatasetManager();
      return;
    }

    var retryImport = target.closest(".builder-retry-import");
    if (retryImport) {
      event.preventDefault();
      event.stopPropagation();
      send("retry_import", {
        id: retryImport.dataset.importId,
        nonce: Date.now(),
      });
      return;
    }

    var removeImport = target.closest(".builder-remove-import");
    if (removeImport) {
      event.preventDefault();
      event.stopPropagation();
      send("remove_import", {
        id: removeImport.dataset.importId,
        nonce: Date.now(),
      });
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
    var example = target.closest(".example-btn");
    if (example) {
      showClientLoadingWorkbench(
        example.dataset.label || "Selected example",
        "Waiting to load…"
      );
      send("use_example", example.dataset.ex);
    }
  });

  document.addEventListener("keydown", function (event) {
    var fileTrigger = event.target.closest(".builder-file-trigger");
    if (
      fileTrigger &&
      (event.key === "Enter" || event.key === " ")
    ) {
      event.preventDefault();
      var fileInput = document.getElementById(fileTrigger.getAttribute("for"));
      if (fileInput) fileInput.click();
      return;
    }
    var enhanceCheckbox = event.target.closest(".enhance-module-checkbox");
    if (
      enhanceCheckbox &&
      event.key === "Enter" &&
      !enhanceCheckbox.disabled
    ) {
      event.preventDefault();
      enhanceCheckbox.click();
      return;
    }
    if (!isTextInput(event.target)) {
      var modifier = event.ctrlKey || event.metaKey;
      if (modifier && event.key === "Enter") {
        event.preventDefault();
        var build = document.getElementById("build");
        if (build && !build.disabled) build.click();
        return;
      }
      if (modifier && event.code === "KeyO") {
        event.preventDefault();
        var add = document.getElementById("dataset_files");
        if (add) add.click();
        return;
      }
      if (modifier && event.code === "KeyZ") {
        var undo = document.getElementById("undo_remove");
        if (undo) {
          event.preventDefault();
          undo.click();
        }
        return;
      }
    }
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
    if (event.target.matches(".group-color-search")) {
      filterGroupColors(event.target);
      return;
    }
    if (event.target.matches('input[type="color"]')) {
      enhanceColour(event.target);
    }
  });

  document.addEventListener("change", function (event) {
    if (event.target.matches("#dataset_files")) {
      beginClientDatasetUpload(event.target);
      return;
    }
    if (event.target.matches(".group-color-input")) {
      updateGroupColor(event.target);
      return;
    }
    if (!event.target.matches(".enhance-table-display-name")) return;
    send("enhance-table_action", {
      action: "rename",
      key: event.target.dataset.tableKey,
      name: event.target.value,
      nonce: Date.now(),
    });
  });

  function messageValues(value) {
    if (value === null || typeof value === "undefined" || value === "") return [];
    return Array.isArray(value) ? value : [value];
  }

  function updateExampleDirectory(message) {
    var used = new Set(messageValues(message && message.ids));
    var loading = new Set(messageValues(message && message.loading));
    document.querySelectorAll(".example-btn[data-ex]").forEach(function (el) {
      var id = el.dataset.ex;
      var isLoading = loading.has(id);
      var taken = used.has(id) && !isLoading;
      var label = el.querySelector(".ex-label");
      el.classList.toggle("is-loading", isLoading);
      el.classList.toggle("is-taken", taken);
      el.disabled = taken || isLoading;
      el.setAttribute("aria-disabled", taken || isLoading ? "true" : "false");
      if (label) label.textContent = isLoading ? "Loading…" : el.dataset.label;
    });
  }

  function registerExampleMessageHandler() {
    if (exampleMessageHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_used_examples",
      updateExampleDirectory
    );
    exampleMessageHandlerRegistered = true;
  }

  function registerBuildDialogHandler() {
    if (buildDialogHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("builder_build_dialog", showBuildDialog);
    window.Shiny.addCustomMessageHandler("builder_focus_dataset", function (message) {
      var context = document.querySelector(".dataset-context");
      if (!context) return;
      context.scrollIntoView({
        block: "start",
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
      context.focus();
      if (message && message.message) scheduleStatusAnnouncement(message.message);
    });
    window.Shiny.addCustomMessageHandler("builder_focus_review", function (message) {
      var review = document.getElementById("review-stage");
      if (!review) return;
      review.setAttribute("tabindex", "-1");
      review.scrollIntoView({
        block: "start",
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
      review.focus();
      if (message && message.message) scheduleStatusAnnouncement(message.message);
    });
    window.Shiny.addCustomMessageHandler("builder_import_status", function (message) {
      if (message && message.text) scheduleStatusAnnouncement(message.text);
    });
    buildDialogHandlerRegistered = true;
  }

  document.addEventListener("shiny:connected", function () {
    registerExampleMessageHandler();
    registerBuildDialogHandler();
    if (document.body) enhanceDynamicContent();
  });
  document.addEventListener("shiny:sessioninitialized", function () {
    registerExampleMessageHandler();
    registerBuildDialogHandler();
  });

  function initializeBuilder() {
    registerExampleMessageHandler();
    registerBuildDialogHandler();

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
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeBuilder, { once: true });
  } else {
    initializeBuilder();
  }
})();
