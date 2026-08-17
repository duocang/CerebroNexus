/* Cerebro Dataset Builder: semantic client interaction and accessibility. */
(function () {
  "use strict";

  var workflowProgressScrollTimer = null;
  var compactWorkflowManager = window.matchMedia("(max-width: 40rem)");
  var narrowManager = window.matchMedia("(max-width: 58rem)");
  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  function syncWorkflowProgressHeight() {
    var progress = document.querySelector(".builder-workflow-progress");
    var height = progress ? Math.ceil(progress.getBoundingClientRect().height) : 0;
    document.documentElement.style.setProperty(
      "--builder-workflow-progress-height",
      height + "px"
    );
  }
  function updateWorkflowProgressVisibility() {
    var progress = document.querySelector(".builder-workflow-progress");
    if (!progress) return;
    if (
      !compactWorkflowManager.matches ||
      reducedMotion.matches ||
      progress.matches(":focus-within")
    ) {
      progress.classList.remove("is-scrolling");
      return;
    }
    progress.classList.add("is-scrolling");
    window.clearTimeout(workflowProgressScrollTimer);
    workflowProgressScrollTimer = window.setTimeout(function () {
      progress.classList.remove("is-scrolling");
    }, 250);
  }
  window.addEventListener("scroll", updateWorkflowProgressVisibility, { passive: true });
  window.addEventListener("resize", function () {
    var progress = document.querySelector(".builder-workflow-progress");
    if (progress) progress.classList.remove("is-scrolling");
    syncWorkflowProgressHeight();
  });

  var statusTimer = null;
  var lastAnnouncement = "";
  var observedPrimaryAction = null;
  var firstRunKey = "cerebro-builder-first-run-v1";
  var exampleMessageHandlerRegistered = false;
  var buildDialogHandlerRegistered = false;
  var viewerGroupHandlerRegistered = false;
  var viewerProjectionHandlerRegistered = false;
  var viewerTrajectoryHandlerRegistered = false;
  var spatialSectionHandlerRegistered = false;
  var desiredSpatialSection = null;
  var clientUploadSequence = 0;
  var clientImportQueue = [];
  var clientImportFailures = [];
  var activeClientImport = null;
  var uploadConnectionReady = true;
  var importSyncPending = false;
  var serverImportGate = false;
  var clientImportHandlersRegistered = false;
  var viewerDisclosureState = new Map();
  var managerTransitionSequence = 0;
  var datasetMutationsLocked = false;
  var buildStatusScrollPhase = 0;
  var normalMotionDuration = 180;
  var authEditor = {
    nextId: 1,
    committed: [],
    snapshot: [],
    open: false,
    saving: false,
  };

  function send(name, value) {
    if (window.Shiny) {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  function applyDatasetMutationLock() {
    var selectors = [
      "#dataset_files",
      ".builder-file-trigger",
      ".example-btn",
      ".builder-reorder",
      ".builder-drop",
      ".builder-retry-import",
      ".builder-remove-import",
      "#undo_remove",
    ].join(", ");
    document.querySelectorAll(selectors).forEach(function (control) {
      if ("disabled" in control) control.disabled = datasetMutationsLocked;
      control.setAttribute(
        "aria-disabled",
        datasetMutationsLocked ? "true" : "false"
      );
      control.classList.toggle(
        "is-dataset-mutation-locked",
        datasetMutationsLocked
      );
      if (datasetMutationsLocked) control.setAttribute("tabindex", "-1");
      else if (control.classList.contains("builder-file-trigger")) {
        control.setAttribute("tabindex", "0");
      } else {
        control.removeAttribute("tabindex");
      }
    });
  }

  function revealBuildStatus() {
    var host = document.getElementById("build-stage-status");
    if (!host || !host.isConnected) {
      buildStatusScrollPhase = 0;
      return;
    }
    var viewportBottom = window.innerHeight - 16;
    var delta = Math.max(0, Math.ceil(
      host.getBoundingClientRect().bottom - viewportBottom
    ));
    window.scrollBy({
      top: delta,
      behavior: reducedMotion.matches ? "auto" : "smooth",
    });
  }

  function showImmediateBuildStatus() {
    var host = document.getElementById("build-stage-status");
    var output = document.getElementById("build_stage_status_content");
    if (!host || !output) return;

    var previous = host.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    if (previous) previous.remove();

    var section = document.createElement("section");
    section.className = [
      "builder-stage-section",
      "builder-build-status-section",
      "is-client-build-status",
    ].join(" ");
    var heading = document.createElement("h3");
    heading.textContent = "Build status";
    var waiting = document.createElement("div");
    waiting.className = "builder-build-waiting";
    var spinner = document.createElement("span");
    spinner.className = "spinner";
    spinner.setAttribute("aria-hidden", "true");
    var label = document.createElement("span");
    label.textContent = "Preparing build…";
    waiting.append(spinner, label);
    section.append(heading, waiting);
    host.insertBefore(section, output);

    var topbar = document.querySelector(".topbar");
    var topbarBottom = topbar ? topbar.getBoundingClientRect().bottom : 0;
    host.style.scrollMarginTop = Math.max(0, topbarBottom + 12) + "px";
    host.setAttribute("tabindex", "-1");
    buildStatusScrollPhase = 1;
    revealBuildStatus();
    buildStatusScrollPhase = 2;
    host.focus({ preventScroll: true });
    scheduleStatusAnnouncement("Preparing build.");
  }

  function authCopy(accounts) {
    return accounts.map(function (account) {
      return { id: account.id, username: account.username, password: account.password };
    });
  }

  function authAccountRow(account) {
    var row = document.createElement("div");
    row.className = "builder-auth-row";
    row.dataset.authId = account.id;
    [["Username", "text", "builder-auth-username", "username", account.username],
      ["Password", "password", "builder-auth-password", "new-password", account.password]
    ].forEach(function (spec) {
      var label = document.createElement("label");
      label.textContent = spec[0];
      var input = document.createElement("input");
      input.type = spec[1];
      input.className = spec[2];
      input.autocomplete = spec[3];
      input.value = spec[4];
      label.appendChild(input);
      row.appendChild(label);
    });
    var remove = document.createElement("button");
    remove.type = "button";
    remove.className = "btn builder-auth-remove";
    remove.textContent = "Remove";
    row.appendChild(remove);
    return row;
  }

  function authRows() {
    return Array.from(document.querySelectorAll(".builder-auth-row")).map(function (row) {
      return {
        id: row.dataset.authId,
        username: row.querySelector(".builder-auth-username").value,
        password: row.querySelector(".builder-auth-password").value,
      };
    });
  }

  function authRender(accounts) {
    var root = document.querySelector("[data-auth-rows]");
    if (!root) return;
    root.replaceChildren();
    accounts.forEach(function (account) { root.appendChild(authAccountRow(account)); });
  }

  function authNewAccount() {
    return { id: "auth-account-" + authEditor.nextId++, username: "", password: "" };
  }

  function clearAuthLiveInputs() {
    document.querySelectorAll(".builder-auth-row input").forEach(function (input) {
      input.value = "";
    });
  }

  function clearAuthSecrets() {
    clearAuthLiveInputs();
    authEditor.committed = [];
    authEditor.snapshot = [];
  }

  function clearAuthError() {
    var error = document.getElementById("builder-auth-error");
    if (!error) return;
    error.textContent = "";
    error.hidden = true;
  }

  function restoreAuthSnapshot() {
    authEditor.committed = authCopy(authEditor.snapshot);
    authRender(authEditor.snapshot);
  }

  function authOpenFocusFallback() {
    var trigger = document.querySelector(".builder-auth-open");
    var options = trigger && trigger.closest("details");
    if (options) options.open = true;
    return trigger;
  }

  function setAuthSaving(pendingNonce) {
    var saving = typeof pendingNonce === "number" && Number.isFinite(pendingNonce);
    authEditor.saving = saving ? pendingNonce : false;
    var dialog = document.getElementById("builder-auth-dialog");
    if (!dialog) return;
    dialog.querySelectorAll("input, button").forEach(function (control) {
      control.disabled = saving;
    });
  }

  function closeAuthDialog(restore) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    authEditor.open = false;
    backdrop.classList.remove("is-visible");
    dialog.classList.remove("is-visible");
    backdrop.hidden = true;
    document.body.classList.remove("builder-dialog-open");
    if (restore) restoreFocus(dialog);
  }

  function openAuthDialog(trigger) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    clearAuthError();
    setAuthSaving(false);
    authEditor.snapshot = authCopy(authEditor.committed);
    if (!authEditor.committed.length) authRender([authNewAccount()]);
    else authRender(authEditor.committed);
    authEditor.open = true;
    backdrop.hidden = false;
    prepareDialog(
      dialog,
      trigger,
      function () {
        if (authEditor.saving) return;
        restoreAuthSnapshot();
        closeAuthDialog(true);
      },
      authOpenFocusFallback
    );
    showTransientLayer(backdrop, dialog);
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

  function canRestoreFocus(target) {
    return Boolean(
      target &&
      document.contains(target) &&
      !target.disabled &&
      target.getClientRects().length > 0
    );
  }

  function restoreFocus(dialog) {
    var target = dialog && dialog.__builderRestoreFocus;
    var fallback = dialog && dialog.__builderRestoreFocusFallback;
    var lastTarget = null;
    var attempts = 0;
    function attempt() {
      var active = document.activeElement;
      var focusWasLost = !active || active === document.body ||
        active === document.documentElement ||
        (dialog && dialog.contains(active)) || active === lastTarget;
      // Do not steal focus if the user has already moved it elsewhere while a
      // Shiny redraw is settling. Re-check the intended target briefly because
      // a redraw can replace the button after the first successful focus,
      // leaving document.body active again.
      if (!focusWasLost) return;
      var nextTarget = canRestoreFocus(target) ? target : null;
      if (!nextTarget && typeof fallback === "function") {
        nextTarget = fallback();
      }
      if (canRestoreFocus(nextTarget)) {
        nextTarget.focus();
        lastTarget = nextTarget;
        target = nextTarget;
      }
      attempts++;
      if (attempts < 3) {
        window.setTimeout(attempt, attempts === 1 ? 50 : 150);
      }
    }
    window.setTimeout(attempt, 0);
  }

  function removeDatasetFocusFallback() {
    var rail = document.querySelector(".rail");
    if (
      narrowManager.matches &&
      rail &&
      rail.classList.contains("is-manager-open")
    ) {
      var close = rail.querySelector(".rail-manager-close");
      if (canRestoreFocus(close)) return close;
      var managerItems = focusableElements(rail);
      if (managerItems.length) return managerItems[0];
      if (canRestoreFocus(rail)) return rail;
    }
    var fileTrigger = document.querySelector(".builder-file-trigger");
    if (canRestoreFocus(fileTrigger)) return fileTrigger;
    var railItems = rail ? focusableElements(rail) : [];
    return railItems.length ? railItems[0] : null;
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

  function prepareDialog(dialog, trigger, close, restoreFallback) {
    dialog.setAttribute("role", "dialog");
    dialog.setAttribute("aria-modal", "true");
    dialog.setAttribute("tabindex", "-1");
    dialog.__builderRestoreFocus = trigger || document.activeElement;
    dialog.__builderRestoreFocusFallback = restoreFallback;
    dialog.__builderClose = close;
    dialog.addEventListener("keydown", trapDialogKeydown);
    document.body.classList.add("builder-dialog-open");
    window.setTimeout(function () {
      var items = focusableElements(dialog);
      (items[0] || dialog).focus();
    }, 0);
  }

  function showTransientLayer(backdrop, dialog, visibleClass) {
    var previous = backdrop && backdrop.__builderTransientState;
    if (previous && previous.cancel) previous.cancel();
    var state = { closing: false };
    if (backdrop) backdrop.__builderTransientState = state;
    window.requestAnimationFrame(function () {
      if (
        state.closing ||
        !backdrop ||
        backdrop.__builderTransientState !== state ||
        !backdrop.isConnected ||
        !dialog ||
        !dialog.isConnected
      ) return;
      backdrop.classList.add("is-visible");
      dialog.classList.add(visibleClass || "is-visible");
    });
  }

  function removeTransientLayer(
    backdrop,
    dialog,
    visibleClass,
    complete,
    removeBackdrop
  ) {
    var state = backdrop && backdrop.__builderTransientState;
    if (!state) {
      state = { closing: false };
      if (backdrop) backdrop.__builderTransientState = state;
    }
    state.closing = true;
    if (backdrop) backdrop.classList.remove("is-visible");
    if (dialog) dialog.classList.remove(visibleClass || "is-visible");

    var finished = false;
    var timeout = null;
    function cleanup() {
      if (timeout !== null) window.clearTimeout(timeout);
      if (dialog) dialog.removeEventListener("transitionend", onTransitionEnd);
    }
    function finish() {
      if (finished) return;
      finished = true;
      cleanup();
      if (
        backdrop &&
        backdrop.__builderTransientState === state
      ) {
        delete backdrop.__builderTransientState;
      }
      if (removeBackdrop !== false && backdrop) backdrop.remove();
      if (complete) complete();
    }
    function onTransitionEnd(event) {
      if (event.target !== dialog) return;
      finish();
    }
    state.cancel = function () {
      if (finished) return;
      finished = true;
      cleanup();
    };

    if (reducedMotion.matches || window.__builderMotionDuration === 0) {
      finish();
      return;
    }
    if (dialog) dialog.addEventListener("transitionend", onTransitionEnd);
    timeout = window.setTimeout(
      finish,
      window.__builderMotionDuration + 60
    );
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

  function setClientLoadingPaused(paused) {
    var stage = document.querySelector(".builder-loading-stage.is-client");
    if (!stage) return;
    stage.classList.toggle("is-paused", paused);
    var status = stage.querySelector(".builder-loading-status");
    if (paused && status) status.textContent = "Connection lost. Waiting to reconnect…";
  }

  function openDatasetPicker() {
    if (datasetMutationsLocked) return;
    var picker = document.createElement("input");
    picker.type = "file";
    picker.multiple = true;
    var transport = document.getElementById("dataset_files");
    picker.accept = transport ? transport.accept : ".rds,.qs,.qs2";
    picker.addEventListener("change", function () {
      enqueueClientFiles(picker.files);
    }, { once: true });
    picker.click();
  }

  function clientQueueStatus(entry, index) {
    if (entry.state === "paused") return "Connection lost · Waiting to restore the import state…";
    if (entry.state === "unknown") return "Import state could not be restored";
    if (entry.state === "awaiting_dispatch") return "Preparing upload…";
    if (entry.state === "awaiting_upload") return "Waiting to upload…";
    if (entry.state === "dispatching" || entry.state === "uploading") return "Uploading…";
    if (entry.state === "awaiting_accept") return "Waiting for the server…";
    if (entry.state === "error" || entry.state === "rejected") {
      return entry.error || "Could not start this import";
    }
    return "Waiting · " + String(index + 1) + " in queue" +
      (entry.duplicateHint ? " · Possible duplicate" : "");
  }

  function applyClientImportQueueLock() {
    var locked = clientImportQueue.length > 0;
    document.querySelectorAll(".builder-retry-import").forEach(function (control) {
      control.disabled = locked;
      control.setAttribute("aria-disabled", locked ? "true" : "false");
    });
  }

  function renderClientImportQueue() {
    var container = document.getElementById("ds_client_import_queue");
    if (!container) return;
    container.replaceChildren();
    clientImportQueue.concat(clientImportFailures).forEach(function (entry, index) {
      if (entry.serverId && entry === activeClientImport) return;
      var row = document.createElement("div");
      row.className = "ds ds--import ds--client-upload";
      row.dataset.clientImportId = entry.clientId;
      row.dataset.loadState = entry.state;
      var body = document.createElement("span");
      body.className = "ds-body";
      var name = document.createElement("span");
      name.className = "nm";
      name.textContent = entry.name;
      var status = document.createElement("span");
      status.className = "builder-import-status";
      status.textContent = clientQueueStatus(entry, index);
      body.appendChild(name);
      body.appendChild(status);
      row.appendChild(body);
      var dot = document.createElement("span");
      dot.className = "ds-state-dot";
      dot.setAttribute("aria-hidden", "true");
      row.appendChild(dot);
      if (
        entry !== activeClientImport &&
        !entry.serverId &&
        !["error", "rejected"].includes(entry.state)
      ) {
        var cancel = document.createElement("button");
        cancel.type = "button";
        cancel.className = "btn btn-remove-soft builder-cancel-client-import";
        cancel.dataset.clientImportId = entry.clientId;
        cancel.setAttribute("aria-label", "Cancel queued import " + entry.name);
        cancel.textContent = "Cancel";
        row.appendChild(cancel);
      } else if (clientImportFailures.includes(entry)) {
        if (entry.outcome === "error") {
          var retry = document.createElement("button");
          retry.type = "button";
          retry.className = "btn builder-retry-client-import";
          retry.dataset.clientImportId = entry.clientId;
          retry.textContent = "Retry";
          row.appendChild(retry);
        }
        var remove = document.createElement("button");
        remove.type = "button";
        remove.className = "btn btn-remove-soft builder-remove-client-failure";
        remove.dataset.clientImportId = entry.clientId;
        remove.textContent = "Remove";
        row.appendChild(remove);
      }
      container.appendChild(row);
    });
    applyClientImportQueueLock();
  }

  function failClientDispatch(entry, message) {
    entry.state = "error";
    entry.outcome = "error";
    entry.error = message;
    scheduleStatusAnnouncement(entry.name + ". " + message);
    clientImportQueue.shift();
    activeClientImport = null;
    clientImportFailures.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function startFileTransport(entry) {
    var transport = document.getElementById("dataset_files");
    if (!transport) {
      failClientDispatch(entry, "The upload transport is unavailable.");
      return;
    }
    entry.state = "uploading";
    try {
      var transfer = new DataTransfer();
      transfer.items.add(entry.file);
      transport.files = transfer.files;
    } catch (error) {
      failClientDispatch(entry, "This browser could not prepare the upload.");
      return;
    }
    if (
      !transport.files ||
      transport.files.length !== 1 ||
      transport.files[0].name !== entry.file.name ||
      transport.files[0].size !== entry.file.size
    ) {
      failClientDispatch(entry, "This browser could not prepare the selected file.");
      return;
    }
    renderClientImportQueue();
    showClientLoadingWorkbench(entry.name, "Uploading selected file…");
    transport.dispatchEvent(new Event("change", { bubbles: true }));
    entry.state = "awaiting_accept";
    renderClientImportQueue();
  }

  function dispatchFileImport(entry) {
    entry.state = "awaiting_dispatch";
    renderClientImportQueue();
    showClientLoadingWorkbench(entry.name, "Preparing upload…");
    send("builder_client_import_dispatch", {
      client_id: entry.clientId,
      name: entry.name,
      size: entry.size,
      nonce: Date.now(),
    });
  }

  function handleClientImportDispatchReady(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.kind !== "file" || activeClientImport.serverId) return;
    if (!["awaiting_dispatch", "awaiting_upload"].includes(activeClientImport.state)) return;
    startFileTransport(activeClientImport);
  }

  function dispatchExampleImport(entry) {
    entry.state = "awaiting_accept";
    renderClientImportQueue();
    showClientLoadingWorkbench(entry.name, "Waiting to load…");
    send("builder_import_example", {
      example: entry.exampleId,
      client_id: entry.clientId,
      nonce: Date.now(),
    });
  }

  function dispatchNextClientImport() {
    if (!uploadConnectionReady) return;
    if (importSyncPending) return;
    if (serverImportGate) return;
    if (activeClientImport) return;
    if (!clientImportQueue.length) return;
    activeClientImport = clientImportQueue[0];
    if (activeClientImport.kind === "file") dispatchFileImport(activeClientImport);
    else dispatchExampleImport(activeClientImport);
  }

  function enqueueClientFiles(fileList) {
    var files = Array.from(fileList || []);
    files.forEach(function (file) {
      var duplicateHint = clientImportQueue.some(function (queued) {
        return queued.kind === "file" &&
          queued.name === file.name &&
          queued.size === file.size &&
          queued.lastModified === file.lastModified;
      });
      clientUploadSequence += 1;
      clientImportQueue.push({
        clientId: "client-import-" + clientUploadSequence,
        kind: "file",
        file: file,
        name: file.name,
        size: file.size,
        lastModified: file.lastModified,
        exampleId: null,
        serverId: null,
        state: "queued",
        outcome: null,
        error: null,
        duplicateHint: duplicateHint,
      });
    });
    if (!files.length) return;
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function enqueueExample(example) {
    clientUploadSequence += 1;
    clientImportQueue.push({
      clientId: "client-import-" + clientUploadSequence,
      kind: "example",
      file: null,
      name: example.name,
      size: null,
      lastModified: null,
      exampleId: example.exampleId,
      serverId: null,
      state: "queued",
      outcome: null,
      error: null,
    });
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function cancelClientImport(clientId) {
    var index = clientImportQueue.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportQueue[index];
    if (entry === activeClientImport) {
      if (!entry.serverId) return;
      send("cancel_pending_upload", { id: entry.serverId, nonce: Date.now() });
      return;
    }
    clientImportQueue.splice(index, 1);
    renderClientImportQueue();
  }

  function removeClientImportFailure(clientId) {
    clientImportFailures = clientImportFailures.filter(function (entry) {
      return entry.clientId !== clientId;
    });
    renderClientImportQueue();
  }

  function retryClientImportFailure(clientId) {
    var index = clientImportFailures.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportFailures.splice(index, 1)[0];
    clientUploadSequence += 1;
    entry.clientId = "client-import-" + clientUploadSequence;
    entry.state = "queued";
    entry.outcome = null;
    entry.error = null;
    entry.serverId = null;
    clientImportQueue.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportAccepted(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    activeClientImport.serverId = message.server_id || null;
    activeClientImport.state = "reading";
    renderClientImportQueue();
  }

  function handleClientImportRelease(message) {
    if (!activeClientImport || !message) return;
    if (message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.serverId && message.server_id && message.server_id !== activeClientImport.serverId) return;
    var releasedEntry = activeClientImport;
    activeClientImport.outcome = message.outcome;
    activeClientImport.error = message.message || null;
    if (["error", "rejected"].includes(message.outcome)) {
      scheduleStatusAnnouncement(
        activeClientImport.name + ". " +
          (activeClientImport.error || "The import could not be completed.")
      );
    }
    clientImportQueue.shift();
    activeClientImport = null;
    if (["error", "rejected"].includes(message.outcome) && !message.server_id) {
      clientImportFailures.push(releasedEntry);
    }
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportSync(message) {
    var records = Array.isArray(message && message.imports) ? message.imports : [];
    serverImportGate = Boolean(message && message.server_busy);
    if (!activeClientImport) {
      importSyncPending = false;
      dispatchNextClientImport();
      return;
    }
    var entry = activeClientImport;
    var record = records.find(function (item) {
      return item && item.client_id === entry.clientId;
    });
    if (!record) {
      if (!entry.serverId && ["uploading", "awaiting_accept", "paused"].includes(entry.state)) {
        entry.serverId = null;
        entry.state = "queued";
        activeClientImport = null;
        importSyncPending = false;
        renderClientImportQueue();
        dispatchNextClientImport();
        return;
      }
      entry.state = "unknown";
      entry.error = "The server could not match this import after reconnecting.";
      renderClientImportQueue();
      return;
    }
    if (entry.serverId && record.server_id !== entry.serverId) {
      entry.state = "unknown";
      entry.error = "The restored import identity did not match.";
      renderClientImportQueue();
      return;
    }
    entry.serverId = record.server_id || entry.serverId;
    if (["ready", "error", "cancelled", "rejected"].includes(record.state)) {
      importSyncPending = false;
      handleClientImportRelease({
        client_id: entry.clientId,
        server_id: entry.serverId,
        outcome: record.state,
        message: record.message || null,
      });
      return;
    }
    if (record.state === "awaiting_upload") {
      var uploadWasStarted = ["uploading", "awaiting_accept"].includes(entry.stateBeforePause);
      entry.state = uploadWasStarted ? "awaiting_accept" : "awaiting_upload";
      importSyncPending = false;
      renderClientImportQueue();
      if (!uploadWasStarted) startFileTransport(entry);
      return;
    }
    entry.state = record.state || entry.stateBeforePause || "unknown";
    importSyncPending = false;
    renderClientImportQueue();
  }

  function updateDialogLock() {
    var hasVisibleModal = Array.from(
      document.querySelectorAll('[aria-modal="true"]')
    ).some(function (dialog) {
      return !dialog.closest("[hidden]");
    });
    document.body.classList.toggle(
      "builder-dialog-open",
      hasVisibleModal
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
    var readiness = current && current.querySelector(".rail-readiness-status");
    var nextName = name ? name.textContent.trim() : "No dataset selected";
    var nextState = [
      readiness ? readiness.textContent.trim() : "",
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
    managerTransitionSequence += 1;
    var closeSequence = managerTransitionSequence;
    if (summary) summary.setAttribute("aria-expanded", "false");
    rail.removeEventListener("keydown", trapDialogKeydown);
    setRailDesktopSemantics(rail);
    if (narrowManager.matches) rail.setAttribute("aria-hidden", "true");
    restoreFocus(rail);
    removeTransientLayer(
      backdrop,
      rail,
      "is-manager-visible",
      function () {
        if (managerTransitionSequence !== closeSequence) return;
        rail.classList.remove("is-manager-open");
        if (backdrop) backdrop.classList.remove("is-open");
        if (!narrowManager.matches) setRailDesktopSemantics(rail);
        updateDialogLock();
      },
      false
    );
  }

  function openDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !summary || !narrowManager.matches) return;
    managerTransitionSequence += 1;
    rail.classList.add("is-manager-open");
    if (backdrop) backdrop.classList.add("is-open");
    rail.removeAttribute("aria-hidden");
    summary.setAttribute("aria-expanded", "true");
    prepareDialog(rail, summary, closeDatasetManager);
    showTransientLayer(backdrop, rail, "is-manager-visible");
  }

  function applyRailMode() {
    var rail = document.querySelector(".rail");
    if (!rail) return;
    if (narrowManager.matches) {
      if (!rail.classList.contains("is-manager-open")) {
        rail.setAttribute("aria-hidden", "true");
      }
    } else {
      managerTransitionSequence += 1;
      rail.classList.remove("is-manager-open");
      rail.classList.remove("is-manager-visible");
      var backdrop = document.querySelector(".rail-manager-backdrop");
      if (backdrop) {
        var state = backdrop.__builderTransientState;
        if (state && state.cancel) state.cancel();
        delete backdrop.__builderTransientState;
        backdrop.classList.remove("is-open", "is-visible");
      }
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
    confirm.className = "btn btn-remove-soft";
    confirm.textContent = "Remove dataset";
    actions.appendChild(cancel);
    actions.appendChild(confirm);
    dialog.appendChild(title);
    dialog.appendChild(message);
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);

    var closed = false;
    function close(commitRemoval) {
      if (closed) return;
      closed = true;
      if (commitRemoval) {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    cancel.addEventListener("click", function () { close(false); });
    confirm.addEventListener("click", function () {
      close(true);
    });
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, removeDataset, close, removeDatasetFocusFallback);
    showTransientLayer(backdrop, dialog);
  }

  function datasetRailFocusIdentity(element) {
    var row = element && element.closest && element.closest(".ds[data-ds]");
    if (!row) return null;
    var action = element.closest(".builder-pick, .builder-reorder, .builder-drop");
    if (!action) return null;
    return {
      datasetId: row.dataset.ds,
      kind: action.classList.contains("builder-pick") ? "pick" :
        action.classList.contains("builder-reorder") ? "reorder" : "drop",
      direction: action.dataset.direction || null,
    };
  }

  function datasetRailFocusTarget(rail, identity) {
    if (!identity) return null;
    var row = Array.from(rail.querySelectorAll(".ds[data-ds]")).find(function (item) {
      return item.dataset.ds === identity.datasetId;
    });
    if (!row) return null;
    if (identity.kind === "pick") return row.querySelector(".builder-pick");
    if (identity.kind === "drop") return row.querySelector(".builder-drop");
    return Array.from(row.querySelectorAll(".builder-reorder")).find(function (item) {
      return item.dataset.direction === identity.direction;
    }) || null;
  }

  function parseDatasetRailElement(html, expectedId, expectedFingerprint) {
    if (typeof html !== "string" || !html.trim()) return null;
    var template = document.createElement("template");
    template.innerHTML = html.trim();
    if (template.content.childElementCount !== 1) return null;
    var row = template.content.firstElementChild;
    if (
      !row.matches(".ds.ds--ready[data-ds]") ||
      row.dataset.ds !== expectedId ||
      row.dataset.railFingerprint !== expectedFingerprint
    ) return null;
    return row;
  }

  function reconcileDatasetRail(message) {
    var rail = document.getElementById("ds_ready_list");
    var rows = message && message.rows;
    if (!rail || !Array.isArray(rows) || typeof message.empty_html !== "string") return;

    var ids = new Set();
    var target = [];
    for (var index = 0; index < rows.length; index++) {
      var record = rows[index];
      if (
        !record || typeof record.id !== "string" || !record.id ||
        typeof record.fingerprint !== "string" || ids.has(record.id)
      ) {
        console.error("Builder rejected an invalid dataset rail snapshot.");
        return;
      }
      var parsed = parseDatasetRailElement(
        record.html, record.id, record.fingerprint
      );
      if (!parsed) {
        console.error("Builder rejected invalid dataset rail row markup.");
        return;
      }
      ids.add(record.id);
      target.push({ record: record, parsed: parsed });
    }

    var empty = null;
    if (!target.length) {
      var emptyTemplate = document.createElement("template");
      emptyTemplate.innerHTML = message.empty_html.trim();
      empty = emptyTemplate.content.childElementCount === 1 ?
        emptyTemplate.content.firstElementChild : null;
      if (!empty || !empty.matches(".rail-empty")) {
        console.error("Builder rejected invalid empty dataset rail markup.");
        return;
      }
    }

    var focusedElement = document.activeElement;
    var focusIdentity = datasetRailFocusIdentity(focusedElement);
    var existing = new Map();
    rail.querySelectorAll(":scope > .ds[data-ds]").forEach(function (row) {
      existing.set(row.dataset.ds, row);
    });

    if (!target.length) {
      rail.replaceChildren(empty);
    } else {
      rail.querySelectorAll(":scope > .rail-empty").forEach(function (node) {
        node.remove();
      });
      target.forEach(function (item) {
        var row = existing.get(item.record.id);
        if (!row || row.dataset.railFingerprint !== item.record.fingerprint) {
          if (row) row.replaceWith(item.parsed);
          row = item.parsed;
        }
        rail.appendChild(row);
        existing.delete(item.record.id);
      });
      existing.forEach(function (row) { row.remove(); });
    }

    var nextFocus = datasetRailFocusTarget(rail, focusIdentity);
    if (nextFocus && focusedElement !== nextFocus) nextFocus.focus();
    document.querySelectorAll(".builder-confirm-dialog").forEach(function (dialog) {
      var identity = datasetRailFocusIdentity(dialog.__builderRestoreFocus);
      var replacement = datasetRailFocusTarget(rail, identity);
      if (replacement) dialog.__builderRestoreFocus = replacement;
    });
    updateRailSummary();
  }

  function importRailFocusIdentity(element) {
    var row = element && element.closest && element.closest(".ds[data-import-id]");
    var action = element && element.closest && element.closest(
      ".builder-pick-import, .builder-retry-import, .builder-remove-import"
    );
    if (!row || !action) return null;
    return {
      importId: row.dataset.importId,
      action: action.classList.contains("builder-pick-import") ? "pick" :
        action.classList.contains("builder-retry-import") ? "retry" : "remove",
    };
  }

  function importRailFocusTarget(rail, identity) {
    if (!identity) return null;
    var row = Array.from(rail.querySelectorAll(".ds[data-import-id]")).find(
      function (item) { return item.dataset.importId === identity.importId; }
    );
    if (!row) return null;
    var selector = identity.action === "pick" ? ".builder-pick-import" :
      identity.action === "retry" ? ".builder-retry-import" :
        ".builder-remove-import";
    return row.querySelector(selector);
  }

  function parseImportRailElement(html, expectedId, expectedFingerprint) {
    if (typeof html !== "string" || !html.trim()) return null;
    var template = document.createElement("template");
    template.innerHTML = html.trim();
    if (template.content.childElementCount !== 1) return null;
    var row = template.content.firstElementChild;
    if (
      !row.matches(".ds.ds--import[data-import-id]") ||
      row.dataset.importId !== expectedId ||
      row.dataset.importFingerprint !== expectedFingerprint
    ) return null;
    return row;
  }

  function reconcileImportRail(message) {
    var host = document.getElementById("ds_import_list");
    var rows = message && message.rows;
    if (!host || !Array.isArray(rows)) return;

    var ids = new Set();
    var target = [];
    for (var index = 0; index < rows.length; index++) {
      var record = rows[index];
      if (
        !record || typeof record.id !== "string" || !record.id ||
        typeof record.fingerprint !== "string" || ids.has(record.id)
      ) {
        console.error("Builder rejected an invalid import rail snapshot.");
        return;
      }
      var parsed = parseImportRailElement(
        record.html, record.id, record.fingerprint
      );
      if (!parsed) {
        console.error("Builder rejected invalid import rail row markup.");
        return;
      }
      ids.add(record.id);
      target.push({ record: record, parsed: parsed });
    }

    var focused = document.activeElement;
    var focusIdentity = importRailFocusIdentity(focused);
    var list = host.querySelector(":scope > .builder-import-list");
    if (!target.length) {
      host.replaceChildren();
      updateRailSummary();
      return;
    }
    if (!list) {
      list = document.createElement("div");
      list.className = "builder-import-list";
      list.setAttribute("aria-label", "Datasets being added");
      host.replaceChildren(list);
    }
    var existing = new Map();
    list.querySelectorAll(":scope > .ds[data-import-id]").forEach(function (row) {
      existing.set(row.dataset.importId, row);
    });
    target.forEach(function (item) {
      var row = existing.get(item.record.id);
      if (!row || row.dataset.importFingerprint !== item.record.fingerprint) {
        if (row) row.replaceWith(item.parsed);
        row = item.parsed;
      }
      list.appendChild(row);
      existing.delete(item.record.id);
    });
    existing.forEach(function (row) { row.remove(); });
    var nextFocus = importRailFocusTarget(list, focusIdentity);
    if (nextFocus && nextFocus !== focused) nextFocus.focus();
    applyDatasetMutationLock();
    updateRailSummary();
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

    var closed = false;
    function close() {
      if (closed) return;
      closed = true;
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    closeButton.addEventListener("click", close);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, infoButton, close);
    showTransientLayer(backdrop, dialog);
  }

  function setMarkerDialog(message) {
    var backdrop = document.getElementById("builder-marker-dialog-backdrop");
    var dialog = document.getElementById("builder-marker-dialog");
    var title = document.getElementById("builder-marker-dialog-title");
    var closeButton = document.getElementById("builder-marker-dialog-close");
    if (!backdrop || !dialog || !closeButton) return;

    function close() {
      if (backdrop.hidden) return;
      dialog.removeEventListener("keydown", trapDialogKeydown);
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        backdrop.hidden = true;
        updateDialogLock();
        restoreFocus(dialog);
      }, false);
    }

    if (!message || message.action === "close") {
      close();
      return;
    }
    if (title && message.title) title.textContent = message.title;
    backdrop.hidden = false;
    closeButton.onclick = close;
    backdrop.onclick = function (event) {
      if (event.target === backdrop) close();
    };
    dialog.removeEventListener("keydown", trapDialogKeydown);
    prepareDialog(
      dialog,
      document.querySelector(".marker-genes-action"),
      close
    );
    showTransientLayer(backdrop, dialog);
  }

  function showBuildDialog(message) {
    var existing = document.querySelector(".builder-build-dialog-backdrop");
    if (message && message.action === "close") {
      if (existing) existing.remove();
      updateDialogLock();
      return;
    }
    if (!message || message.type !== "conflict") return;
    if (existing) return;
    var trigger = document.getElementById("build");
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop builder-build-dialog-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog builder-build-dialog";
    var title = document.createElement("h2");
    title.id = "builder-build-dialog-title";
    title.textContent = message.title || "Files already exist";
    dialog.appendChild(title);

    var description = document.createElement("p");
    description.textContent = "Some outputs already exist in this folder:";
    dialog.appendChild(description);

    var list = document.createElement("ul");
    list.className = "builder-build-dialog-list";
    var values = message.files || [];
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
    var question = document.createElement("p");
    question.textContent = "What would you like to do?";
    dialog.appendChild(question);

    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions builder-build-dialog-actions";
    var buttons = [
      { label: "Cancel", action: "cancel", className: "btn" },
      { label: "Replace existing files", action: "replace", className: "btn btn-replace" },
      { label: "Choose another folder", action: "choose_another", className: "btn btn-action" },
    ];
    var closed = false;
    function close(action) {
      if (closed) return;
      closed = true;
      send("builder_build_dialog", {
        action: action || "cancel",
        nonce: message.nonce,
      });
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
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
    showTransientLayer(backdrop, dialog);
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
        var terminal = state === "complete" && step.dataset.step === "complete";
        var current = step.dataset.step === state && !terminal;
        var complete = state !== "failure" && index >= 0 &&
          (index < activeIndex || terminal);
        step.classList.toggle("is-complete", complete);
        step.classList.toggle("is-current", current);
        step.classList.toggle("is-terminal", terminal);
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
      .getPropertyValue("--duration-normal")
      .trim();
    var match = duration.match(/^((?:\d+(?:\.\d+)?|\.\d+))(ms|s)$/);
    if (!match) {
      window.__builderMotionDuration = normalMotionDuration;
      return;
    }
    var value = Number(match[1]);
    var milliseconds = match[2] === "ms" ? value : value * 1000;
    window.__builderMotionDuration = Number.isFinite(milliseconds) ?
      Math.round(milliseconds) : normalMotionDuration;
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
      syncSpatialPreviewAspect(plot);
      renderPlotSummary(plot);
      plot.classList.remove("is-updating");
    });
    plot.on("plotly_relayouting", function () {
      plot.classList.add("is-updating");
    });
    syncSpatialPreviewAspect(plot);
    renderPlotSummary(plot);
  }

  function finiteExtent(values) {
    var finite = Array.from(values || []).map(Number).filter(Number.isFinite);
    if (!finite.length) return null;
    return Math.max.apply(null, finite) - Math.min.apply(null, finite);
  }

  function spatialPreviewAspect(plot) {
    var images = plot && plot.layout && Array.from(plot.layout.images || []);
    if (images && images.length) {
      var image = images[0];
      var imageWidth = Math.abs(Number(image.sizex));
      var imageHeight = Math.abs(Number(image.sizey));
      if (imageWidth > 0 && imageHeight > 0) return imageWidth / imageHeight;
    }
    var xs = [], ys = [];
    Array.from((plot && plot.data) || []).forEach(function (trace) {
      xs = xs.concat(Array.from(trace.x || []));
      ys = ys.concat(Array.from(trace.y || []));
    });
    var width = finiteExtent(xs);
    var height = finiteExtent(ys);
    return width > 0 && height > 0 ? width / height : 4 / 3;
  }

  function syncSpatialPreviewAspect(plot) {
    var frame = plot && plot.closest(".spatial-alignment-plot-frame");
    if (!frame) return;
    var ratio = spatialPreviewAspect(plot);
    ratio = Math.max(0.75, Math.min(3, ratio));
    var value = ratio.toFixed(4);
    if (frame.dataset.spatialPreviewAspect === value) return;
    frame.dataset.spatialPreviewAspect = value;
    frame.style.setProperty("--spatial-preview-aspect", value);
    syncSpatialWorkbench(frame, plot, ratio);
    if (window.Plotly && window.Plotly.Plots) {
      window.requestAnimationFrame(function () {
        if (plot.isConnected && plot.offsetParent !== null && plot.clientWidth > 0) {
          window.Plotly.Plots.resize(plot);
        }
      });
    }
  }

  function syncSpatialWorkbench(frame, plot, ratio) {
    var workbench = frame && frame.closest(".spatial-alignment-workbench");
    var figure = frame && frame.closest(".spatial-alignment-figure");
    if (!workbench || !figure) return;
    var desktop = window.matchMedia("(min-width: 68.8125rem)").matches;
    if (!desktop) {
      frame.style.removeProperty("width");
      frame.style.removeProperty("height");
      return;
    }
    frame.style.width = "100%";
    frame.style.height = "100%";
    if (!frame.builderResizeObserver && window.ResizeObserver) {
      frame.builderResizeObserver = new ResizeObserver(function () {
        window.requestAnimationFrame(function () {
          syncSpatialWorkbench(frame, plot, spatialPreviewAspect(plot));
          if (window.Plotly && window.Plotly.Plots && plot.isConnected &&
              plot.offsetParent !== null && plot.clientWidth > 0) {
            window.Plotly.Plots.resize(plot);
          }
        });
      });
      frame.builderResizeObserver.observe(figure);
    }
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

  function viewerGroupRows(root) {
    return Array.from(root.querySelectorAll(".viewer-group-row"));
  }

  function updateViewerMetadataSelection(root, emit) {
    if (!root) return;
    var retained = viewerGroupRows(root)
      .filter(function (row) {
        var checkbox = row.querySelector(".viewer-metadata-retain");
        return checkbox && checkbox.checked;
      })
      .map(function (row) { return row.dataset.group; });
    if (emit && root.dataset.metadataInputId) {
      send(root.dataset.metadataInputId, {
        action: "set-retention",
        retained: retained,
        nonce: Date.now(),
      });
    }
  }

  function selectViewerMetadata(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-metadata-retain");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all-supported" ||
        row.dataset.recommendedRetained === "true";
    });
    updateViewerMetadataSelection(root, true);
  }

  function updateDefaultCopy(root, selector) {
    root.querySelectorAll(selector).forEach(function (input) {
      var label = input.closest("label");
      var copy = label && label.querySelector(".viewer-default-copy");
      if (copy) copy.textContent = input.checked ? "Default" : "Set default";
    });
  }

  function updateViewerGroupCount(root) {
    var count = root.querySelectorAll(".viewer-group-include:checked").length;
    var card = root.closest(".builder-viewer-card");
    var output = card && card.querySelector("[data-viewer-group-count]");
    var selected = root.querySelector(".viewer-group-default:checked");
    var row = selected && selected.closest(".viewer-group-row");
    var label = row && row.querySelector(".viewer-group-name");
    if (output) {
      output.textContent = count + " included · Default: " +
        (label ? label.textContent.trim() : "None");
    }
  }

  function updateViewerGroupSelection(root, emit) {
    if (!root) return;
    var rows = viewerGroupRows(root);
    var included = rows
      .filter(function (row) {
        var checkbox = row.querySelector(".viewer-group-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (row) { return row.dataset.group; });
    if (!included.length) {
      var first = rows.find(function (row) {
        return row.dataset.eligible === "true";
      });
      var firstCheckbox = first && first.querySelector(".viewer-group-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.group];
      }
    }
    var currentDefault = root.querySelector(".viewer-group-default:checked");
    var defaultGroup = currentDefault && included.includes(currentDefault.value)
      ? currentDefault.value
      : included[0] || null;
    rows.forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      var isIncluded = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      row.classList.toggle("is-included", isIncluded);
      if (radio) {
        radio.disabled = !isIncluded;
        radio.checked = isIncluded && row.dataset.group === defaultGroup;
      }
    });
    updateDefaultCopy(root, ".viewer-group-default");
    updateViewerGroupCount(root);
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set-groups",
        included: included,
        default: defaultGroup,
        nonce: Date.now(),
      });
    }
  }

  function focusViewerGroup(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    viewerGroupRows(root).forEach(function (row) {
      var focus = row.querySelector(".viewer-group-focus");
      var selected = row.dataset.group === button.dataset.group;
      row.classList.toggle("is-focused", selected);
      if (focus) focus.setAttribute("aria-pressed", selected ? "true" : "false");
    });
    if (root.dataset.focusInputId) {
      send(root.dataset.focusInputId, {
        group: button.dataset.group,
        nonce: Date.now(),
      });
    }
  }

  function filterViewerGroups(search) {
    var root = search.closest(".viewer-group-workspace");
    if (!root) return;
    var query = search.value.trim().toLowerCase();
    viewerGroupRows(root).forEach(function (row) {
      row.hidden = query.length > 0 && !row.dataset.search.includes(query);
    });
  }

  function selectViewerGroups(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all" || row.dataset.suggested === "true";
    });
    updateViewerGroupSelection(root, true);
  }

  function setupViewerGroupCatalogs() {
    document.querySelectorAll(".viewer-group-workspace").forEach(function (root) {
      if (root.dataset.builderGroups === "true") return;
      root.dataset.builderGroups = "true";
      updateViewerGroupSelection(root, false);
      var initial = root.querySelector(".viewer-group-default:checked");
      var row = initial && initial.closest(".viewer-group-row");
      var focus = row && row.querySelector(".viewer-group-focus");
      if (focus) {
        row.classList.add("is-focused");
        focus.setAttribute("aria-pressed", "true");
      }
    });
  }

  function applyViewerGroupState(message) {
    var root = document.querySelector(".viewer-group-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(row.dataset.group);
      if (radio) radio.checked = row.dataset.group === (message && message.default);
    });
    updateViewerGroupSelection(root, false);
    var status = root.querySelector(".viewer-group-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function projectionCards(root) {
    return Array.from(root.querySelectorAll(".viewer-projection-card"));
  }

  function updateProjectionSelection(root, emit) {
    if (!root) return;
    var cards = projectionCards(root);
    var included = cards
      .filter(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (card) { return card.dataset.projection; });
    if (!included.length) {
      var first = cards.find(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && !checkbox.disabled;
      });
      var firstCheckbox = first && first.querySelector(".viewer-projection-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.projection];
      }
    }
    var selectedDefault = root.querySelector(".viewer-projection-default:checked");
    var defaultProjection = selectedDefault && included.includes(selectedDefault.value)
      ? selectedDefault.value
      : included[0] || null;
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var radio = card.querySelector(".viewer-projection-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (radio) {
        radio.disabled = !selected;
        radio.checked = selected && card.dataset.projection === defaultProjection;
      }
    });
    updateDefaultCopy(root, ".viewer-projection-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-projection-count]");
    var defaultCard = cards.find(function (card) {
      return card.dataset.projection === defaultProjection;
    });
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    if (countOutput) {
      countOutput.textContent = included.length + " included · Default: " +
        (defaultLabel ? defaultLabel.textContent.trim() : "None");
    }
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: included,
        default: defaultProjection,
        nonce: Date.now(),
      });
    }
  }

  function updateProjectionPointSize(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var radius = Math.max(0, Math.min(4.5, value * 0.34));
    var output = root.querySelector(".viewer-point-size-value");
    if (output) output.textContent = String(value);
    root.querySelectorAll(".viewer-projection-preview").forEach(function (svg) {
      svg.dataset.pointSize = String(value);
      svg.querySelectorAll(".viewer-scatter-point").forEach(function (point) {
        point.setAttribute("r", radius.toFixed(2));
      });
    });
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function updateProjectionCellPercentage(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var output = root.querySelector(".viewer-cell-percentage-value");
    if (output) output.textContent = String(value) + "%";
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function trajectoryCards(root) {
    return Array.from(root.querySelectorAll(".viewer-trajectory-card"));
  }

  function trajectoryRecord(card) {
    return { method: card.dataset.method, name: card.dataset.trajectory };
  }

  function updateTrajectorySelection(root, emit) {
    if (!root) return;
    var cards = trajectoryCards(root);
    var includedCards = cards.filter(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      return checkbox && checkbox.checked && !checkbox.disabled;
    });
    var selectedDefault = root.querySelector(".viewer-trajectory-default:checked");
    var defaultCard = selectedDefault && selectedDefault.closest(".viewer-trajectory-card");
    if (!defaultCard || !includedCards.includes(defaultCard)) {
      defaultCard = includedCards[0] || null;
    }
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (radio) {
        radio.disabled = !selected;
        radio.checked = selected && card === defaultCard;
      }
    });
    updateDefaultCopy(root, ".viewer-trajectory-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-trajectory-count]");
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    if (countOutput) {
      countOutput.textContent = includedCards.length + " included" +
        (defaultLabel ? " · Default: " + defaultLabel.textContent.trim() : "");
    }
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: includedCards.map(trajectoryRecord),
        default: defaultCard ? trajectoryRecord(defaultCard) : null,
        nonce: Date.now(),
      });
    }
  }

  function setupViewerContentCatalogs() {
    document.querySelectorAll(".viewer-projection-workspace").forEach(function (root) {
      if (root.dataset.builderProjections === "true") return;
      root.dataset.builderProjections = "true";
      updateProjectionSelection(root, false);
      var pointSize = root.querySelector(".viewer-point-size-input");
      if (pointSize) updateProjectionPointSize(pointSize, false);
      var cellPercentage = root.querySelector(".viewer-cell-percentage-input");
      if (cellPercentage) updateProjectionCellPercentage(cellPercentage, false);
    });
    document.querySelectorAll(".viewer-trajectory-workspace").forEach(function (root) {
      if (root.dataset.builderTrajectories === "true") return;
      root.dataset.builderTrajectories = "true";
      updateTrajectorySelection(root, false);
    });
  }

  function disclosureStateKey(details) {
    var stage = details.closest(".builder-stage-core");
    var dataset = stage && stage.querySelector(".builder-rendered-for-input");
    var datasetId = dataset && dataset.value ? dataset.value : "builder";
    return datasetId + "::" + details.dataset.disclosureKey;
  }

  function setupPersistentDisclosures() {
    document.querySelectorAll("details[data-disclosure-key]").forEach(function (details) {
      if (details.dataset.builderDisclosure === "true") return;
      var key = disclosureStateKey(details);
      if (viewerDisclosureState.has(key)) {
        details.open = viewerDisclosureState.get(key);
      }
      details.dataset.builderDisclosure = "true";
      details.addEventListener("toggle", function () {
        viewerDisclosureState.set(key, details.open);
      });
    });
  }

  function setupViewerContentAccordions() {
    document.querySelectorAll(".builder-viewer-content").forEach(function (root) {
      if (root.dataset.builderAccordion === "true") return;
      root.dataset.builderAccordion = "true";
      var cards = Array.from(root.querySelectorAll(".builder-viewer-card"));
      cards.forEach(function (card) {
        card.addEventListener("toggle", function () {
          if (!card.open) return;
          cards.forEach(function (sibling) {
            if (sibling !== card) sibling.open = false;
          });
        });
      });
    });
  }

  function applyViewerProjectionState(message) {
    var root = document.querySelector(".viewer-projection-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    projectionCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var radio = card.querySelector(".viewer-projection-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.projection);
      if (radio) radio.checked = card.dataset.projection === (message && message.default);
    });
    var pointSize = root.querySelector(".viewer-point-size-input");
    if (pointSize && message && Number.isFinite(Number(message.point_size))) {
      pointSize.value = String(message.point_size);
      updateProjectionPointSize(pointSize, false);
    }
    var cellPercentage = root.querySelector(".viewer-cell-percentage-input");
    if (
      cellPercentage &&
      message &&
      Number.isFinite(Number(message.percentage_cells_to_show))
    ) {
      cellPercentage.value = String(message.percentage_cells_to_show);
      updateProjectionCellPercentage(cellPercentage, false);
    }
    updateProjectionSelection(root, false);
    var status = root.querySelector(".viewer-projection-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function applyViewerTrajectoryState(message) {
    var root = document.querySelector(".viewer-trajectory-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included).map(function (record) {
      return record.method + "::" + record.name;
    }));
    var defaultKey = message && message.default
      ? message.default.method + "::" + message.default.name
      : null;
    trajectoryCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.trajectoryKey);
      if (radio) radio.checked = card.dataset.trajectoryKey === defaultKey;
    });
    updateTrajectorySelection(root, false);
    var status = root.querySelector(".viewer-trajectory-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function normalizeCreatableSelectValue(value) {
    return String(value || "").trim().replace(/\s+/g, " ");
  }

  function setupCreatableSelect(root) {
    if (!root || root.dataset.builderCreatableSelectReady === "true") return;
    var select = root.querySelector("select");
    var selectize = select && select.selectize;
    var dropdown = selectize && selectize.$dropdown && selectize.$dropdown[0];
    if (!selectize || !dropdown) return;

    var inputLabel = root.dataset.builderCreateInputLabel || "Custom value";
    var actionLabel = root.dataset.builderCreateActionLabel || "Add custom value";
    var maximumLength = parseInt(root.dataset.builderCreateMaxlength, 10) || 80;
    var row = document.createElement("div");
    row.className = "builder-creatable-select-row";
    var input = document.createElement("input");
    input.type = "text";
    input.className = "builder-creatable-select-input";
    input.placeholder = root.dataset.builderCreatePlaceholder || "Type another value";
    input.setAttribute("aria-label", inputLabel);
    input.setAttribute("autocomplete", "off");
    var add = document.createElement("button");
    add.type = "button";
    add.className = "builder-creatable-select-add";
    add.textContent = "Add";
    add.setAttribute("aria-label", actionLabel);
    add.disabled = true;
    var error = document.createElement("p");
    error.className = "builder-creatable-select-error";
    error.setAttribute("aria-live", "polite");
    error.hidden = true;
    row.append(input, add, error);
    dropdown.appendChild(row);

    function updateState() {
      var value = normalizeCreatableSelectValue(input.value);
      var tooLong = value.length > maximumLength;
      add.disabled = !value || tooLong;
      error.hidden = !tooLong;
      error.textContent = tooLong ?
        "Use " + maximumLength + " characters or fewer." : "";
      input.setAttribute("aria-invalid", tooLong ? "true" : "false");
    }

    function matchingOption(value) {
      var comparison = value.toLocaleLowerCase();
      return Object.keys(selectize.options).find(function (key) {
        var option = selectize.options[key] || {};
        return [
          key,
          option[selectize.settings.valueField],
          option[selectize.settings.labelField],
        ].some(function (candidate) {
          return normalizeCreatableSelectValue(candidate).toLocaleLowerCase() === comparison;
        });
      });
    }

    function commitValue() {
      var value = normalizeCreatableSelectValue(input.value);
      if (!value || value.length > maximumLength) {
        updateState();
        return;
      }
      var existing = matchingOption(value);
      var selectedValue = existing || value;
      if (!existing) {
        var option = {};
        option[selectize.settings.valueField] = value;
        option[selectize.settings.labelField] = value;
        selectize.addOption(option);
      }
      selectize.setValue(selectedValue);
      selectize.close();
      input.value = "";
      updateState();
      window.setTimeout(function () {
        var controlInput = selectize.$control_input && selectize.$control_input[0];
        if (canRestoreFocus(controlInput)) controlInput.focus();
      }, 0);
    }

    row.addEventListener("mousedown", function (event) {
      event.stopPropagation();
    });
    input.addEventListener("mousedown", function () {
      selectize.ignoreFocus = true;
    });
    input.addEventListener("focus", function () {
      selectize.ignoreFocus = false;
      selectize.isFocused = true;
      selectize.isBlurring = false;
      selectize.$control.addClass("focus");
    });
    input.addEventListener("input", updateState);
    input.addEventListener("keydown", function (event) {
      if (event.key !== "Enter") return;
      event.preventDefault();
      event.stopPropagation();
      commitValue();
    });
    add.addEventListener("click", commitValue);
    root.dataset.builderCreatableSelectReady = "true";
  }

  function setupCreatableSelects() {
    document.querySelectorAll("[data-builder-creatable-select='true']").forEach(
      setupCreatableSelect
    );
  }

  var multiSelectPlaceholder = "Select…";

  function minimumMultiSelectEmptyWidth() {
    var more = document.getElementById("cv-more-btn");
    return more ? Math.ceil(more.getBoundingClientRect().width) : 140;
  }

  function multiSelectEmptyWidth(select, instance) {
    var control = instance.$control && instance.$control[0];
    var style = window.getComputedStyle(control || select);
    var canvas = multiSelectEmptyWidth.canvas ||
      (multiSelectEmptyWidth.canvas = document.createElement("canvas"));
    var context = canvas.getContext("2d");
    context.font = style.font || [style.fontSize, style.fontFamily].join(" ");
    var longest = Array.prototype.reduce.call(select.options, function (width, option) {
      return Math.max(width, context.measureText(option.textContent || "").width);
    }, 0);
    return Math.min(
      window.innerWidth - 32,
      Math.max(minimumMultiSelectEmptyWidth(), Math.ceil(longest + 42))
    );
  }

  function sizeEmptyMultiSelect(select, instance) {
    var empty = !instance.items.length;
    instance.$wrapper.toggleClass("cerebro-multiselect-empty", empty);
    instance.$wrapper.css(
      "width",
      empty ? multiSelectEmptyWidth(select, instance) + "px" : ""
    );
  }

  function enhanceMultiSelect(select) {
    if (!select || !select.multiple) return;
    select.setAttribute("data-placeholder", multiSelectPlaceholder);
    if (!select.selectize) return;
    select.selectize.settings.placeholder = multiSelectPlaceholder;
    select.selectize.$control_input.attr("placeholder", multiSelectPlaceholder);
    select.selectize.updatePlaceholder();
    select.selectize.$wrapper.addClass("cerebro-multiselect");
    sizeEmptyMultiSelect(select, select.selectize);
    if (!select.dataset.cerebroMultiSelectReady) {
      select.selectize.on("change", function () {
        sizeEmptyMultiSelect(select, select.selectize);
      });
      select.dataset.cerebroMultiSelectReady = "true";
    }
  }

  function spatialAlignmentHasImage(sidebar) {
    var imageControls = sidebar.querySelector(
      ".spatial-alignment-sidebar-scroll > .shiny-panel-conditional"
    );
    return Boolean(
      imageControls && window.getComputedStyle(imageControls).display !== "none"
    );
  }

  function syncSpatialAlignmentScrollbar(sidebar) {
    if (!sidebar.dataset.builderWheelPageScroll) {
      sidebar.dataset.builderWheelPageScroll = "true";
      sidebar.addEventListener("wheel", function (event) {
        if (event.ctrlKey) return;
        var multiplier = event.deltaMode === 1
          ? 16
          : event.deltaMode === 2
            ? window.innerHeight
            : 1;
        event.preventDefault();
        window.scrollBy(0, event.deltaY * multiplier);
      }, { passive: false });
    }
    var scrollbar = sidebar.querySelector(".spatial-alignment-persistent-scrollbar");
    if (!scrollbar) {
      scrollbar = document.createElement("div");
      scrollbar.className = "spatial-alignment-persistent-scrollbar";
      scrollbar.setAttribute("aria-hidden", "true");
      scrollbar.innerHTML = '<div class="spatial-alignment-persistent-scrollbar-thumb"></div>';
      sidebar.appendChild(scrollbar);

      scrollbar.addEventListener("pointerdown", function (event) {
        event.preventDefault();
        var start = event.clientY;
        var initialScroll = sidebar.scrollTop;
        var available = Math.max(1, sidebar.scrollHeight - sidebar.clientHeight);
        var thumb = scrollbar.firstElementChild;
        var track = Math.max(1, scrollbar.clientHeight - thumb.clientHeight);
        function drag(moveEvent) {
          sidebar.scrollTop = initialScroll +
            ((moveEvent.clientY - start) / track) * available;
        }
        function stop() {
          document.removeEventListener("pointermove", drag);
          document.removeEventListener("pointerup", stop);
        }
        document.addEventListener("pointermove", drag);
        document.addEventListener("pointerup", stop, { once: true });
      });
      sidebar.addEventListener("scroll", function () {
        syncSpatialAlignmentScrollbar(sidebar);
      }, { passive: true });
    }

    var visible = spatialAlignmentHasImage(sidebar);
    scrollbar.hidden = !visible;
    if (!visible) return;

    var rect = sidebar.getBoundingClientRect();
    var trackHeight = Math.max(0, rect.height);
    var range = Math.max(0, sidebar.scrollHeight - sidebar.clientHeight);
    var thumbHeight = range > 0
      ? Math.max(28, trackHeight * (sidebar.clientHeight / sidebar.scrollHeight))
      : trackHeight;
    var travel = Math.max(0, trackHeight - thumbHeight);
    var thumbTop = range > 0 ? travel * (sidebar.scrollTop / range) : 0;
    var thumb = scrollbar.firstElementChild;

    scrollbar.style.top = rect.top + "px";
    scrollbar.style.left = (rect.right - 10) + "px";
    scrollbar.style.height = trackHeight + "px";
    thumb.style.height = thumbHeight + "px";
    thumb.style.transform = "translateY(" + thumbTop + "px)";
  }

  function syncSpatialAlignmentScrollbars() {
    document.querySelectorAll(".spatial-alignment-sidebar").forEach(
      syncSpatialAlignmentScrollbar
    );
  }

  function enhanceDynamicContent() {
    syncWorkflowProgressHeight();
    syncSpatialAlignmentScrollbars();
    if (window.BuilderIcons) window.BuilderIcons.decorate(document);
    setupRail();
    updateRailSummary();
    registerStages();
    registerPrimaryAction();
    updateStatusSemantics();
    updatePipelines();
    var buildStatusHost = document.getElementById("build-stage-status");
    var clientStatus = buildStatusHost && buildStatusHost.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    var serverStatus = document.querySelector(
      "#build_stage_status_content .builder-build-status-section"
    );
    if (clientStatus && serverStatus) clientStatus.remove();
    if (
      buildStatusScrollPhase === 2 &&
      document.querySelector(
        "#build-stage-status .builder-build-status-section:not(.is-client-build-status)"
      )
    ) {
      revealBuildStatus();
      buildStatusScrollPhase = 0;
    }
    setupFirstRun();
    if (document.querySelector(".result-card.success")) {
      var guide = document.querySelector(".builder-first-run");
      if (guide) guide.hidden = true;
      try { window.localStorage.setItem(firstRunKey, "dismissed"); } catch (error) {}
    }
    updateDialogLock();
    setupPersistentDisclosures();
    setupViewerContentAccordions();
    setupViewerGroupCatalogs();
    setupViewerContentCatalogs();
    setupCreatableSelects();
    if (desiredSpatialSection) {
      var sectionSelect = document.getElementById("enhance-active_section");
      if (sectionSelect && sectionSelect.value !== desiredSpatialSection) {
        if (sectionSelect.selectize) {
          sectionSelect.selectize.setValue(desiredSpatialSection, true);
        } else {
          sectionSelect.value = desiredSpatialSection;
        }
      }
    }
    applyDatasetMutationLock();
    applyClientImportQueueLock();
    document.querySelectorAll(".js-plotly-plot").forEach(enhancePlot);
    document.querySelectorAll('input[type="color"]').forEach(enhanceColour);
    document.querySelectorAll("select[multiple]").forEach(enhanceMultiSelect);
  }

  document.addEventListener("click", function (event) {
    var target = event.target;
    var spatialImageTrigger = target.closest(".enhance-tissue-file-button");
    if (spatialImageTrigger) {
      var spatialImageInput = document.getElementById(
        spatialImageTrigger.getAttribute("for")
      );
      if (spatialImageInput) spatialImageInput.value = "";
    }
    if (target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
    if (target.closest("#build")) showImmediateBuildStatus();
    var authOpen = target.closest(".builder-auth-open");
    if (authOpen) {
      event.preventDefault();
      openAuthDialog(authOpen);
      return;
    }
    if (target.closest(".builder-auth-add")) {
      event.preventDefault();
      var rows = authRows();
      rows.push(authNewAccount());
      authRender(rows);
      var lastUsername = document.querySelector(".builder-auth-row:last-child .builder-auth-username");
      if (lastUsername) lastUsername.focus();
      return;
    }
    var authRemove = target.closest(".builder-auth-remove");
    if (authRemove) {
      event.preventDefault();
      authRemove.closest(".builder-auth-row").remove();
      return;
    }
    if (target.closest(".builder-auth-cancel")) {
      event.preventDefault();
      if (authEditor.saving) return;
      restoreAuthSnapshot();
      closeAuthDialog(true);
      return;
    }
    if (target.closest(".builder-auth-save")) {
      event.preventDefault();
      if (authEditor.saving) return;
      var accounts = authRows();
      var users = accounts.map(function (account) { return account.username.trim(); });
      var valid = accounts.length && users.every(Boolean) &&
        new Set(users).size === users.length &&
        accounts.every(function (account) { return account.password.length >= 8; });
      var authError = document.getElementById("builder-auth-error");
      if (!valid) {
        if (authError) {
          authError.textContent = "Add unique usernames and passwords of at least 8 characters.";
          authError.hidden = false;
        }
        return;
      }
      accounts.forEach(function (account, index) { account.username = users[index]; });
      authEditor.committed = authCopy(accounts);
      var nonce = Date.now();
      setAuthSaving(nonce);
      send("builder_auth_accounts", { enabled: true, accounts: accounts, nonce: nonce });
      return;
    }
    if (target.matches("#build_require_login") && !target.checked) {
      clearAuthSecrets();
      authRender([]);
      send("builder_auth_accounts", { enabled: false, accounts: [], nonce: Date.now() });
      send("builder_auth_accounts", null);
      return;
    }
    var groupColorToggle = target.closest(".group-color-toggle");
    if (groupColorToggle) {
      event.preventDefault();
      toggleGroupColors(groupColorToggle);
      return;
    }
    var viewerGroupFocus = target.closest(".viewer-group-focus");
    if (viewerGroupFocus) {
      event.preventDefault();
      focusViewerGroup(viewerGroupFocus);
      return;
    }
    var viewerGroupSelect = target.closest(".viewer-group-select");
    if (viewerGroupSelect) {
      event.preventDefault();
      selectViewerGroups(viewerGroupSelect);
      return;
    }
    var viewerMetadataSelect = target.closest(".viewer-metadata-select");
    if (viewerMetadataSelect) {
      event.preventDefault();
      selectViewerMetadata(viewerMetadataSelect);
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
    var confirmMarkerSource = target.closest(".marker-source-confirm");
    if (confirmMarkerSource) {
      event.preventDefault();
      send("enhance-marker_source_confirm", {
        id: confirmMarkerSource.dataset.sourceId,
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
      if (clientImportQueue.length) return;
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

    var cancelClientImportButton = target.closest(".builder-cancel-client-import");
    if (cancelClientImportButton) {
      cancelClientImport(cancelClientImportButton.dataset.clientImportId);
      return;
    }

    var removeClientFailure = target.closest(".builder-remove-client-failure");
    if (removeClientFailure) {
      removeClientImportFailure(removeClientFailure.dataset.clientImportId);
      return;
    }

    var retryClientFailure = target.closest(".builder-retry-client-import");
    if (retryClientFailure) {
      retryClientImportFailure(retryClientFailure.dataset.clientImportId);
      return;
    }

    if (target.closest("#builder_add_datasets")) {
      openDatasetPicker();
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
      enqueueExample({
        exampleId: example.dataset.ex,
        name: example.dataset.label || "Selected example",
      });
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      return;
    }
    var fileTrigger = event.target.closest(".builder-file-trigger");
    if (
      fileTrigger &&
      (event.key === "Enter" || event.key === " ")
    ) {
      event.preventDefault();
      if (fileTrigger.id === "builder_add_datasets") openDatasetPicker();
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
        openDatasetPicker();
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
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-group-search")) {
      filterViewerGroups(event.target);
      return;
    }
    if (event.target.matches(".group-color-search")) {
      filterGroupColors(event.target);
      return;
    }
    if (event.target.matches('input[type="color"]')) {
      enhanceColour(event.target);
    }
  });

  document.addEventListener("change", function (event) {
    if (event.target.id === "enhance-active_section") {
      desiredSpatialSection = event.target.value;
    }
    if (event.target.id.indexOf("enhance-marker_source_mode_") === 0) {
      send("enhance-marker_source_mode", {
        id: event.target.id.replace("enhance-marker_source_mode_", ""),
        mode: event.target.value,
        nonce: Date.now(),
      });
      return;
    }
    if (event.target.matches("#dataset_files")) return;
    if (event.target.matches(".viewer-group-include")) {
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-metadata-retain")) {
      updateViewerMetadataSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-group-default")) {
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-projection-include, .viewer-projection-default")) {
      updateProjectionSelection(
        event.target.closest(".viewer-projection-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-trajectory-include, .viewer-trajectory-default")) {
      updateTrajectorySelection(
        event.target.closest(".viewer-trajectory-workspace"),
        true
      );
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
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_rail_patch",
      reconcileDatasetRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_rail_patch",
      reconcileImportRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_mutation_lock",
      function (message) {
        datasetMutationsLocked = Boolean(message && message.locked === true);
        applyDatasetMutationLock();
      }
    );
    window.Shiny.addCustomMessageHandler("builder_marker_dialog", setMarkerDialog);
    window.Shiny.addCustomMessageHandler("builder_focus_stage", function (message) {
      var id = message && message.id;
      if (["upload", "configure", "review", "build"].indexOf(id) < 0) return;
      var stage = document.querySelector('[data-workflow-stage="' + id + '"]');
      if (!stage) return;
      var heading = stage.querySelector("h2");
      if (!heading) return;
      var topbar = document.querySelector(".topbar");
      var topbarBottom = topbar ? topbar.getBoundingClientRect().bottom : 0;
      heading.style.scrollMarginTop = Math.max(0, topbarBottom + 12) + "px";
      heading.setAttribute("tabindex", "-1");
      heading.scrollIntoView({
        block: "start",
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
      heading.focus({ preventScroll: true });
      scheduleStatusAnnouncement("Opened " + id + " step.");
    });
    window.Shiny.addCustomMessageHandler("builder_import_status", function (message) {
      if (message && message.text) scheduleStatusAnnouncement(message.text);
    });
    function handleAuthStatus(message) {
      if (
        !message ||
        typeof authEditor.saving !== "number" ||
        message.nonce !== authEditor.saving
      ) return;
      var error = document.getElementById("builder-auth-error");
      setAuthSaving(false);
      if (message.ok) {
        if (error) { error.textContent = ""; error.hidden = true; }
        clearAuthLiveInputs();
        authRender([]);
        authEditor.snapshot = [];
        send("builder_auth_accounts", null);
        closeAuthDialog(true);
      } else if (error) {
        error.textContent = "Login accounts could not be saved.";
        error.hidden = false;
      }
    }
    window.__builderHandleAuthStatus = handleAuthStatus;
    window.Shiny.addCustomMessageHandler("builder_auth_status", handleAuthStatus);
    window.Shiny.addCustomMessageHandler("builder_auth_reset", function (message) {
      if (!message || message.reset !== true) return;
      setAuthSaving(false);
      clearAuthSecrets();
      clearAuthError();
      authRender([]);
      send("builder_auth_accounts", null);
    });
    buildDialogHandlerRegistered = true;
  }

  function registerClientImportHandlers() {
    if (clientImportHandlersRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_dispatch_ready",
      handleClientImportDispatchReady
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_accepted",
      handleClientImportAccepted
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_release",
      handleClientImportRelease
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_sync",
      handleClientImportSync
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_scheduler_state",
      function (message) {
        serverImportGate = Boolean(message && message.active);
        if (!serverImportGate) dispatchNextClientImport();
      }
    );
    clientImportHandlersRegistered = true;
  }

  function registerViewerGroupHandler() {
    if (viewerGroupHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_group_state",
      applyViewerGroupState
    );
    viewerGroupHandlerRegistered = true;
  }

  function registerViewerContentHandlers() {
    if (!window.Shiny) return;
    if (!viewerProjectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_projection_state",
        applyViewerProjectionState
      );
      viewerProjectionHandlerRegistered = true;
    }
    if (!viewerTrajectoryHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_trajectory_state",
        applyViewerTrajectoryState
      );
      viewerTrajectoryHandlerRegistered = true;
    }
    if (!spatialSectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_spatial_section_state",
        function (message) {
          if (!message || !message.value) return;
          desiredSpatialSection = message.value;
          [0, 50, 200, 500, 1000, 2000, 5000].forEach(function (delay) {
            window.setTimeout(function () {
              var select = document.getElementById("enhance-active_section");
              if (!select) return;
              if (select.selectize) select.selectize.setValue(message.value, true);
              else select.value = message.value;
            }, delay);
          });
        }
      );
      spatialSectionHandlerRegistered = true;
    }
  }

  function requestClientImportSync() {
    uploadConnectionReady = true;
    importSyncPending = true;
    registerExampleMessageHandler();
    registerBuildDialogHandler();
    registerClientImportHandlers();
    registerViewerGroupHandler();
    registerViewerContentHandlers();
    send("builder_dataset_rail_sync", { nonce: Date.now() });
    send("builder_import_rail_sync", { nonce: Date.now() });
    send("builder_import_sync_request", { nonce: Date.now() });
    if (document.body) enhanceDynamicContent();
  }

  document.addEventListener("shiny:connected", function () {
    setClientLoadingPaused(false);
    requestClientImportSync();
  });
  document.addEventListener("shiny:disconnected", function () {
    uploadConnectionReady = false;
    importSyncPending = true;
    clientImportQueue.forEach(function (entry) {
      entry.stateBeforePause = entry.state;
      entry.state = "paused";
    });
    setClientLoadingPaused(true);
    renderClientImportQueue();
    scheduleStatusAnnouncement(
      "Connection lost. Waiting to restore the import state."
    );
  });
  document.addEventListener("shiny:sessioninitialized", function () {
    requestClientImportSync();
  });
  document.addEventListener("shiny:conditional", function () {
    window.requestAnimationFrame(syncSpatialAlignmentScrollbars);
  });
  function initializeBuilder() {
    registerExampleMessageHandler();
    registerBuildDialogHandler();
    registerClientImportHandlers();
    registerViewerGroupHandler();
    registerViewerContentHandlers();

    var datasetTrigger = document.getElementById("builder_add_datasets");
    if (datasetTrigger) {
      datasetTrigger.addEventListener("dragover", function (event) {
        event.preventDefault();
        datasetTrigger.classList.add("is-drag-over");
      });
      datasetTrigger.addEventListener("dragleave", function () {
        datasetTrigger.classList.remove("is-drag-over");
      });
      datasetTrigger.addEventListener("drop", function (event) {
        event.preventDefault();
        datasetTrigger.classList.remove("is-drag-over");
        enqueueClientFiles(event.dataTransfer && event.dataTransfer.files);
      });
    }

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
    window.addEventListener("resize", syncSpatialAlignmentScrollbars, { passive: true });
    window.addEventListener("scroll", syncSpatialAlignmentScrollbars, { passive: true });
    ensureLiveRegion();
    enhanceDynamicContent();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeBuilder, { once: true });
  } else {
    initializeBuilder();
  }
})();
