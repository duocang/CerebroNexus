/* Portable Linked views configuration dialog and Shiny transport. */
(function () {
  'use strict';

  var pending = null;
  var sequence = 0;
  var lastFocus = null;
  var exportReady = false;
  var exportBusy = false;
  var pendingSnapshotName = null;
  var pendingTimer = null;
  var SNAPSHOT_KEY = 'cerebro.linked-views.snapshots.v1';
  var SNAPSHOT_LIMIT = 12;
  var SNAPSHOT_BYTES = 4 * 1024 * 1024;

  function byId(id) { return document.getElementById(id); }
  function adapter() { return window.cerebroLinkedViewsState || null; }
  function nextNonce() {
    sequence += 1;
    return 'cv-config-' + Date.now().toString(36) + '-' + sequence.toString(36);
  }
  function status(message, tone) {
    var element = byId('cv-config-status');
    if (!element) return;
    element.replaceChildren();
    if (tone === 'error' && message) {
      var title = document.createElement('strong');
      title.className = 'cv-config-status-title';
      title.textContent = 'Unable to open this configuration';
      var detail = document.createElement('span');
      detail.textContent = message;
      element.appendChild(title);
      element.appendChild(detail);
    } else {
      element.textContent = message;
    }
    element.classList.toggle('is-error', tone === 'error');
    element.classList.toggle('is-success', tone === 'success');
    element.classList.toggle('is-working', tone === 'working');
  }
  function clearPending() {
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = null;
    pending = null;
    setBusy(false);
  }
  function startPending(nonce, action) {
    pending = { nonce: nonce, action: action };
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = window.setTimeout(function () {
      if (!pending || pending.nonce !== nonce) return;
      clearPending();
      if (action === 'save') pendingSnapshotName = null;
      setUploadLoading(false);
      status(
        action === 'apply'
          ? 'Restore did not finish. Reload this page and try again.'
          : 'Saving did not finish. Reload this page and try again.',
        'error'
      );
    }, 10000);
  }
  function snapshotName(value) {
    return typeof value === 'string' ? value.trim().replace(/\s+/g, ' ').slice(0, 80) : '';
  }
  function currentFingerprint() {
    try {
      var state = adapter();
      var config = state && state.ready() ? state.capture() : null;
      return config && config.dataset && typeof config.dataset.cell_fingerprint === 'string'
        ? config.dataset.cell_fingerprint : null;
    } catch (ignore) { return null; }
  }
  function appVersion() {
    var node = document.querySelector('.cerebro-brand-version');
    return node ? String(node.textContent || '').trim() : '';
  }
  function readSnapshots() {
    try {
      var stored = window.localStorage && window.localStorage.getItem(SNAPSHOT_KEY);
      var parsed = stored ? JSON.parse(stored) : { records: [] };
      return Array.isArray(parsed.records) ? parsed.records.filter(function (record) {
        return record && typeof record.id === 'string' && typeof record.name === 'string' &&
          typeof record.saved_at === 'string' && typeof record.json === 'string';
      }) : [];
    } catch (ignore) { return []; }
  }
  function writeSnapshots(records) {
    var sorted = records.slice().sort(function (a, b) {
      return String(b.saved_at).localeCompare(String(a.saved_at));
    }).slice(0, SNAPSHOT_LIMIT);
    var document = { version: 1, records: sorted };
    var text = JSON.stringify(document);
    while (sorted.length && text.length > SNAPSHOT_BYTES) {
      sorted.pop(); document.records = sorted; text = JSON.stringify(document);
    }
    if (!sorted.length && records.length) throw new Error('This view is too large to save in this browser.');
    try { window.localStorage.setItem(SNAPSHOT_KEY, text); }
    catch (error) { throw new Error('This browser has no space left for saved views.'); }
    return sorted;
  }
  function recordFingerprint(record) {
    try {
      var config = JSON.parse(record.json);
      return config && config.dataset && typeof config.dataset.cell_fingerprint === 'string'
        ? config.dataset.cell_fingerprint : null;
    } catch (ignore) { return null; }
  }
  function visibleSnapshots() {
    var fingerprint = currentFingerprint();
    if (!fingerprint) return [];
    return readSnapshots().filter(function (record) {
      return recordFingerprint(record) === fingerprint;
    });
  }
  function snapshotDate(value) {
    var date = new Date(value);
    return isNaN(date.getTime()) ? '' : date.toLocaleString();
  }
  function snapshotButton(label, action, record) {
    var button = document.createElement('button');
    button.type = 'button'; button.className = 'cv-snapshot-action';
    button.textContent = label;
    button.addEventListener('click', function () { action(record); });
    return button;
  }
  function renderSnapshots() {
    var list = byId('cv-snapshot-list');
    var save = byId('cv-snapshot-save');
    if (save) {
      save.disabled = exportBusy || !exportReady;
      save.title = exportReady ? '' : 'Select at least one cell before saving this view';
    }
    if (!list) return;
    list.replaceChildren();
    var records = visibleSnapshots();
    if (!records.length) {
      var empty = document.createElement('p');
      empty.className = 'cv-snapshot-empty';
      empty.textContent = 'No saved views for this cell population yet.';
      list.appendChild(empty);
      return;
    }
    records.forEach(function (record) {
      var row = document.createElement('div'); row.className = 'cv-snapshot-row';
      var details = document.createElement('div'); details.className = 'cv-snapshot-details';
      var name = document.createElement('strong'); name.textContent = record.name;
      var time = document.createElement('span'); time.textContent = snapshotDate(record.saved_at);
      details.appendChild(name); details.appendChild(time); row.appendChild(details);
      var actions = document.createElement('div'); actions.className = 'cv-snapshot-actions';
      actions.appendChild(snapshotButton('Open', restoreSnapshot, record));
      actions.appendChild(snapshotButton('Download', downloadSnapshot, record));
      actions.appendChild(snapshotButton('Rename', renameSnapshot, record));
      actions.appendChild(snapshotButton('Delete', deleteSnapshot, record));
      row.appendChild(actions); list.appendChild(row);
    });
  }
  function refreshExportControls() {
    ['cv-config-download', 'cv-config-copy'].forEach(function (id) {
      var button = byId(id);
      if (!button) return;
      button.disabled = exportBusy || !exportReady;
      button.title = exportReady
        ? ''
        : 'Select at least one cell before exporting this view';
    });
  }
  function setBusy(busy) {
    exportBusy = !!busy;
    refreshExportControls();
    renderSnapshots();
  }
  function setUploadLoading(loading) {
    var upload = byId('coordviews_config_upload');
    var host = upload && upload.closest('.cv-config-upload');
    if (host) host.classList.toggle('is-uploading', !!loading);
  }
  function setReady(ready, selectedCells) {
    var button = byId('cv-config-open');
    if (!button) return;
    var enabled = !!ready;
    exportReady = enabled && Number(selectedCells) > 0;
    refreshExportControls();
    renderSnapshots();
    button.disabled = !enabled;
    button.setAttribute('aria-disabled', enabled ? 'false' : 'true');
    button.title = !ready
      ? 'Linked views is waiting for a data set'
      : 'Download, copy, or open a linked workspace JSON file';
  }
  function openDialog() {
    var dialog = byId('cv-config-dialog');
    if (!dialog || dialog.open) return;
    status('');
    setUploadLoading(false);
    renderSnapshots();
    lastFocus = document.activeElement;
    dialog.showModal();
    var close = byId('cv-config-close');
    if (close) close.focus();
  }
  function closeDialog() {
    var dialog = byId('cv-config-dialog');
    if (dialog && dialog.open) dialog.close();
    status('');
    setUploadLoading(false);
  }
  function request(action) {
    var state = adapter();
    if (!state || !state.ready()) {
      status('Linked views is not ready to save.', 'error');
      return;
    }
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    if (!exportReady) {
      status('Select at least one cell before exporting this view.', 'error');
      return;
    }
    try {
      var nonce = nextNonce();
      startPending(nonce, action);
      setBusy(true);
      status(action === 'copy' ? 'Preparing JSON to copy…' :
        (action === 'save' ? 'Saving this view…' : 'Preparing your download…'));
      Shiny.setInputValue('coordviews_config_request', {
        nonce: nonce,
        action: action,
        config: state.capture()
      }, { priority: 'event' });
    } catch (error) {
      clearPending();
      status(error && error.message ? error.message : 'The view could not be saved.', 'error');
    }
  }
  function fallbackCopy(text) {
    var textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.setAttribute('readonly', 'readonly');
    textarea.style.cssText = 'position:fixed;left:-9999px;top:0;opacity:0';
    document.body.appendChild(textarea);
    textarea.focus();
    textarea.select();
    var copied = false;
    try { copied = document.execCommand('copy'); } catch (ignore) { copied = false; }
    textarea.remove();
    return Promise.resolve(copied);
  }
  function copyText(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      return navigator.clipboard.writeText(text).then(function () { return true; })
        .catch(function () { return fallbackCopy(text); });
    }
    return fallbackCopy(text);
  }
  function finishCopy(result) {
    if (typeof result.json !== 'string') {
      status('The JSON was validated, but could not be copied. Use Download JSON.', 'error');
      return;
    }
    copyText(result.json).then(function (copied) {
      status(
        copied
          ? 'Copied the configuration for ' + result.selected_cells + ' selected cells.'
          : 'Clipboard access was blocked. Use Download JSON instead.',
        copied ? 'success' : 'error'
      );
    });
  }
  function finishDownload(result) {
    if (typeof result.json !== 'string') {
      status('The JSON was validated, but could not be downloaded. Try again.', 'error');
      return;
    }
    var link = null;
    var url = null;
    try {
      if (typeof window.Blob !== 'function' || !window.URL ||
        typeof window.URL.createObjectURL !== 'function') {
        throw new Error('Object URL downloads are unavailable');
      }
      var blob = new window.Blob(
        [result.json],
        { type: 'application/json;charset=utf-8' }
      );
      url = window.URL.createObjectURL(blob);
      link = document.createElement('a');
      link.href = url;
      link.download = typeof result.filename === 'string' && result.filename
        ? result.filename
        : 'linked-views.json';
      link.style.display = 'none';
      document.body.appendChild(link);
      link.click();
      status(
        'Downloaded the configuration for ' + result.selected_cells + ' selected cells.',
        'success'
      );
    } catch (ignore) {
      status('The download could not start. Try Download JSON again.', 'error');
    } finally {
      if (link && link.parentNode) link.parentNode.removeChild(link);
      if (url) {
        window.setTimeout(function () {
          try {
            if (window.URL && typeof window.URL.revokeObjectURL === 'function') {
              window.URL.revokeObjectURL(url);
            }
          } catch (ignore) { /* the document may already be closing */ }
        }, 1000);
      }
    }
  }
  function finishApply(result) {
    try {
      var summary = adapter().apply(result.config, result.colour_data || null);
      status('Restored ' + summary.selectedCells + ' selected cells and view settings.', 'success');
    } catch (error) {
      status(
        error && error.message
          ? error.message
          : 'This configuration uses a view that is unavailable here.',
        'error'
      );
    }
  }
  function finishSave(result) {
    if (typeof result.json !== 'string' || !pendingSnapshotName) {
      status('This view could not be saved.', 'error');
      return;
    }
    try {
      var records = readSnapshots().filter(function (record) { return record.name !== pendingSnapshotName; });
      records.unshift({
        id: nextNonce(), name: pendingSnapshotName,
        saved_at: new Date().toISOString(), app_version: appVersion(), json: result.json
      });
      writeSnapshots(records);
      renderSnapshots();
      status('Saved “' + pendingSnapshotName + '” in this browser.', 'success');
    } catch (error) {
      status(error && error.message ? error.message : 'This view could not be saved.', 'error');
    } finally { pendingSnapshotName = null; }
  }
  function saveSnapshot() {
    var name = snapshotName(window.prompt('Name this saved view:', ''));
    if (!name) return;
    pendingSnapshotName = name;
    request('save');
  }
  function restoreSnapshot(record) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    var nonce = nextNonce();
    if (pending) return;
    startPending(nonce, 'apply');
    setBusy(true); status('Restoring “' + record.name + '”…', 'working');
    Shiny.setInputValue('coordviews_config_request', {
      nonce: nonce, action: 'apply', config_json: record.json
    }, { priority: 'event' });
  }
  function downloadSnapshot(record) {
    var selected = 0;
    try { selected = (JSON.parse(record.json).selection.cells || []).length; } catch (ignore) { /* validated on restore */ }
    finishDownload({ json: record.json, selected_cells: selected,
      filename: 'linked-view-' + record.name.replace(/[^a-z0-9]+/gi, '-').replace(/^-|-$/g, '') + '.json' });
  }
  function renameSnapshot(record) {
    var name = snapshotName(window.prompt('Rename saved view:', record.name));
    if (!name || name === record.name) return;
    try {
      writeSnapshots(readSnapshots().map(function (item) {
        return item.id === record.id ? { id: item.id, name: name, saved_at: item.saved_at, json: item.json } : item;
      }));
      renderSnapshots();
    } catch (error) { status(error.message, 'error'); }
  }
  function deleteSnapshot(record) {
    try {
      writeSnapshots(readSnapshots().filter(function (item) { return item.id !== record.id; }));
      renderSnapshots(); status('Deleted “' + record.name + '”.', 'success');
    } catch (error) { status(error.message, 'error'); }
  }
  function receive(result) {
    if (!result || !pending || String(result.nonce) !== pending.nonce) return;
    var action = pending.action;
    clearPending();
    if (result.action !== action) {
      if (action === 'save') pendingSnapshotName = null;
      status('This page did not receive the expected response. Reload and try again.', 'error');
      return;
    }
    if (action === 'apply') {
      var upload = byId('coordviews_config_upload');
      if (upload) upload.value = '';
      setUploadLoading(false);
    }
    if (!result.ok) {
      if (action === 'save') pendingSnapshotName = null;
      status(result.message || 'The configuration could not be opened.', 'error');
      return;
    }
    if (action === 'copy' && result.action === 'copy') finishCopy(result);
    else if (action === 'download' && result.action === 'download') finishDownload(result);
    else if (action === 'apply' && result.action === 'apply') finishApply(result);
    else if (action === 'save' && result.action === 'save') finishSave(result);
  }
  function beginUpload() {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var nonce = nextNonce();
    startPending(nonce, 'apply');
    setBusy(true);
    setUploadLoading(true);
    status('');
    Shiny.setInputValue('coordviews_config_upload_nonce', nonce, {
      priority: 'event'
    });
  }
  function boot() {
    var open = byId('cv-config-open');
    var dialog = byId('cv-config-dialog');
    if (!open || !dialog) return;

    open.addEventListener('click', openDialog);
    byId('cv-config-close').addEventListener('click', closeDialog);
    byId('cv-config-copy').addEventListener('click', function () { request('copy'); });
    byId('cv-config-download').addEventListener('click', function () {
      request('download');
    });
    var save = byId('cv-snapshot-save');
    if (save) save.addEventListener('click', saveSnapshot);
    var upload = byId('coordviews_config_upload');
    if (upload) upload.addEventListener('change', beginUpload);
    dialog.addEventListener('close', function () {
      status('');
      setUploadLoading(false);
      var target = lastFocus && document.contains(lastFocus) ? lastFocus : open;
      lastFocus = null;
      if (target) target.focus();
    });
    document.addEventListener('keydown', function (event) {
      if (event.key !== 'Escape' || !dialog.open) return;
      event.preventDefault();
      closeDialog();
    });
    window.addEventListener('cerebro:linkedviews-ready', function (event) {
      var detail = event.detail || {};
      setReady(!!detail.ready, detail.selectedCells);
    });
    window.addEventListener('cerebro:linkedviews-selection', function (event) {
      var state = adapter();
      var summary = state && state.summary ? state.summary() : null;
      setReady(!!(state && state.ready()), summary && summary.selectedCells);
    });
    var state = adapter();
    var summary = state && state.summary ? state.summary() : null;
    setReady(!!(state && state.ready()), summary && summary.selectedCells);
    renderSnapshots();

    if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('coordviews_config_result', receive);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
