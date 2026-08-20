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
  var snapshotNameRequest = null;
  var pendingShare = null;
  var pendingShareTimer = null;
  var shareUrlHandled = false;
  var SNAPSHOT_KEY = 'cerebro.linked-views.snapshots.v1';
  var SHARE_KEY = 'cerebro.linked-views.share-receipts.v1';
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
  function readShares() {
    try {
      var stored = window.localStorage && window.localStorage.getItem(SHARE_KEY);
      var parsed = stored ? JSON.parse(stored) : { records: [] };
      return Array.isArray(parsed.records) ? parsed.records.filter(function (record) {
        return record && typeof record.token === 'string' && typeof record.receipt === 'string' &&
          typeof record.expires_at === 'string' && typeof record.fingerprint === 'string';
      }) : [];
    } catch (ignore) { return []; }
  }
  function writeShares(records) {
    window.localStorage.setItem(SHARE_KEY, JSON.stringify({ version: 1, records: records.slice(0, 30) }));
  }
  function shareUrl(token) {
    var url = new URL(window.location.href);
    url.searchParams.set('linked_view', token);
    return url.toString();
  }
  function randomShareToken() {
    if (!window.crypto || !window.crypto.getRandomValues || !window.btoa) return null;
    var bytes = new Uint8Array(32);
    window.crypto.getRandomValues(bytes);
    var binary = '';
    bytes.forEach(function (value) { binary += String.fromCharCode(value); });
    return window.btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
  }
  function removeShareRecord(token) {
    if (!token) return;
    writeShares(readShares().filter(function (item) { return item.token !== token; }));
    renderShares();
  }
  function renderShares() {
    var list = byId('cv-share-list');
    var create = byId('cv-share-create');
    var shareRegion = byId('cv-config-share');
    if (shareRegion) shareRegion.classList.toggle('is-disabled', exportBusy || !exportReady);
    if (create) create.disabled = exportBusy || !exportReady;
    if (!list) return;
    list.replaceChildren();
    var fingerprint = currentFingerprint();
    var records = readShares().filter(function (record) { return record.fingerprint === fingerprint; });
    records.forEach(function (record) {
      var row = document.createElement('div'); row.className = 'cv-share-row';
      var text = document.createElement('span'); text.textContent = 'Expires ' + snapshotDate(record.expires_at);
      var copy = snapshotButton('Copy link', function () {
        copy.disabled = true;
        copy.textContent = 'Copying…';
        copyText(shareUrl(record.token)).then(function (ok) {
          copy.textContent = ok ? 'Copied ✓' : 'Copy failed';
          status(ok ? 'Share link copied.' : 'Clipboard access was blocked.', ok ? 'success' : 'error');
          window.setTimeout(function () {
            copy.textContent = 'Copy link';
            copy.disabled = false;
          }, 1400);
        });
      }, record);
      var revoke = snapshotButton('Revoke', function () { sendShare('share_revoke', record); }, record);
      row.appendChild(text); row.appendChild(copy); row.appendChild(revoke); list.appendChild(row);
    });
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
  function snapshotNeedsColourData(record) {
    try {
      var mode = JSON.parse(record.json).view.colour.mode;
      return mode === '__gene__' || mode === '__rgb__';
    } catch (ignore) { return true; }
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
    var saveRegion = document.querySelector('.cv-config-save-local');
    if (saveRegion) saveRegion.classList.toggle('is-disabled', exportBusy || !exportReady);
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
      var mark = document.createElement('span'); mark.className = 'cv-snapshot-mark';
      mark.setAttribute('aria-hidden', 'true'); mark.textContent = '⌑'; row.appendChild(mark);
      var details = document.createElement('div'); details.className = 'cv-snapshot-details';
      var name = document.createElement('strong'); name.textContent = record.name;
      var time = document.createElement('span'); time.textContent = snapshotDate(record.saved_at);
      details.appendChild(name); details.appendChild(time); row.appendChild(details);
      var primary = document.createElement('div'); primary.className = 'cv-snapshot-primary';
      primary.appendChild(snapshotButton('Open', restoreSnapshot, record)); row.appendChild(primary);
      var actions = document.createElement('div'); actions.className = 'cv-snapshot-actions';
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
    renderShares();
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
    renderShares();
    button.disabled = !enabled;
    button.setAttribute('aria-disabled', enabled ? 'false' : 'true');
    button.title = !ready
      ? 'Linked views is waiting for a data set'
      : 'Save, open, import, export, or share a linked view';
  }
  function sendShare(action, record) {
    var state = adapter();
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue || pendingShare) return;
    if (action === 'share_create' && (!state || !state.ready() || !exportReady)) {
      status('Select at least one cell before creating a share link.', 'error'); return;
    }
    var nonce = nextNonce();
    pendingShare = { nonce: nonce, action: action, payload: null, retried: false };
    if (pendingShareTimer) window.clearTimeout(pendingShareTimer);
    status(action === 'share_open' ? 'Opening shared view…' :
      (action === 'share_revoke' ? 'Revoking share link…' : 'Preparing share link…'), 'working');
    var payload = { nonce: nonce, action: action };
    if (action === 'share_create') {
      payload.config = state.capture();
      var token = randomShareToken(), receipt = randomShareToken();
      if (token && receipt) {
        payload.token = token; payload.receipt = receipt;
        pendingShare.provisionalToken = token;
        var expires = new Date(Date.now() + 90 * 24 * 60 * 60 * 1000).toISOString();
        var records = readShares().filter(function (item) { return item.token !== token; });
        records.unshift({ token: token, receipt: receipt, expires_at: expires,
          fingerprint: currentFingerprint() });
        writeShares(records); renderShares();
        status('Share link ready. Saving in the background…', 'success');
        copyText(shareUrl(token));
      }
    }
    if (record) { payload.token = record.token; if (record.receipt) payload.receipt = record.receipt; }
    pendingShare.payload = payload;
    pendingShareTimer = window.setTimeout(function retryShareRequest() {
      if (!pendingShare || pendingShare.nonce !== nonce) return;
      if (!pendingShare.retried) {
        pendingShare.retried = true;
        Shiny.setInputValue('coordviews_share_request', pendingShare.payload, { priority: 'event' });
        pendingShareTimer = window.setTimeout(retryShareRequest, 4000);
        return;
      }
      removeShareRecord(pendingShare.provisionalToken);
      pendingShare = null; pendingShareTimer = null;
      status('The share link could not be saved. Try again.', 'error');
    }, 2000);
    Shiny.setInputValue('coordviews_share_request', payload, { priority: 'event' });
  }
  function receiveShare(result) {
    if (!result || !pendingShare || String(result.nonce) !== pendingShare.nonce) return;
    var completedShare = pendingShare;
    var action = completedShare.action; pendingShare = null;
    if (pendingShareTimer) window.clearTimeout(pendingShareTimer);
    pendingShareTimer = null;
    if (result.action !== action) {
      status('The page received an unexpected share response.', 'error'); return;
    }
    if (!result.ok) {
      removeShareRecord(completedShare.provisionalToken);
      status(result.message || 'The share request failed.', 'error'); return;
    }
    if (action === 'share_create') {
      var records = readShares().filter(function (item) { return item.token !== result.token; });
      records.unshift({ token: result.token, receipt: result.receipt,
        expires_at: result.expires_at, fingerprint: currentFingerprint() });
      writeShares(records); renderShares();
      if (completedShare.provisionalToken) {
        status('Share link saved. It expires in 90 days.', 'success');
        return;
      }
      status('Share link created. Copying it to your clipboard…', 'success');
      var copyFinished = false;
      var copyNotice = window.setTimeout(function () {
        if (!copyFinished) status('Share link created. Use Copy link below.', 'success');
      }, 1200);
      copyText(shareUrl(result.token)).then(function (ok) {
        copyFinished = true;
        window.clearTimeout(copyNotice);
        status(ok ? 'Share link created and copied. It expires in 90 days.' :
          'Share link created. Use Copy link below.', 'success');
      });
    } else if (action === 'share_revoke') {
      writeShares(readShares().filter(function (item) { return item.token !== result.token; }));
      renderShares(); status('Share link revoked.', 'success');
    } else if (action === 'share_open') {
      finishApply(result);
      var url = new URL(window.location.href); url.searchParams.delete('linked_view');
      window.history.replaceState({}, '', url.toString());
    }
  }
  function openShareFromUrl() {
    if (shareUrlHandled) return;
    var token = new URLSearchParams(window.location.search).get('linked_view');
    if (!token || !adapter() || !adapter().ready()) return;
    shareUrlHandled = true;
    openDialog();
    sendShare('share_open', { token: token });
  }
  function openDialog() {
    var dialog = byId('cv-config-dialog');
    if (!dialog || dialog.open) return;
    status('');
    setUploadLoading(false);
    renderSnapshots();
    renderShares();
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
      return new Promise(function (resolve) {
        var settled = false;
        var timer = null;
        function finish(copied) {
          if (settled) return;
          settled = true;
          if (timer) window.clearTimeout(timer);
          resolve(!!copied);
        }
        function useFallback() {
          fallbackCopy(text).then(finish).catch(function () { finish(false); });
        }
        timer = window.setTimeout(function () { fallbackCopy(text).then(finish); }, 700);
        try {
          navigator.clipboard.writeText(text).then(function () { finish(true); }).catch(useFallback);
        } catch (ignore) { useFallback(); }
      });
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
  function saveSnapshotLocally(name) {
    var state = adapter();
    if (!state || !state.ready()) throw new Error('Linked views is not ready to save.');
    var json = JSON.stringify(state.capture());
    var records = readSnapshots().filter(function (record) { return record.name !== name; });
    records.unshift({ id: nextNonce(), name: name, saved_at: new Date().toISOString(),
      app_version: appVersion(), json: json });
    writeSnapshots(records);
    renderSnapshots();
    status('Saved “' + name + '” on this device.', 'success');
  }
  function openSnapshotNameDialog(mode, record, trigger) {
    var dialog = byId('cv-snapshot-name-dialog'), input = byId('cv-snapshot-name-input');
    if (!dialog || !input) return;
    snapshotNameRequest = { mode: mode, record: record || null, trigger: trigger || document.activeElement };
    byId('cv-snapshot-name-title').textContent = mode === 'rename' ? 'Rename saved view' : 'Save current view';
    byId('cv-snapshot-name-help').textContent = mode === 'rename'
      ? 'Choose a short name that makes this saved view easy to find.'
      : 'Give this view a short name so you can find it later.';
    byId('cv-snapshot-name-confirm').textContent = mode === 'rename' ? 'Rename view' : 'Save view';
    input.value = mode === 'rename' ? record.name : '';
    dialog.showModal(); input.focus(); input.select();
  }
  function closeSnapshotNameDialog() {
    var dialog = byId('cv-snapshot-name-dialog'); if (dialog && dialog.open) dialog.close();
  }
  function confirmSnapshotName() {
    var requestState = snapshotNameRequest, input = byId('cv-snapshot-name-input');
    var name = snapshotName(input && input.value); if (!requestState || !name) return;
    closeSnapshotNameDialog();
    if (requestState.mode === 'save') {
      try { saveSnapshotLocally(name); }
      catch (error) { status(error && error.message ? error.message : 'This view could not be saved.', 'error'); }
      return;
    }
    if (name === requestState.record.name) return;
    try {
      writeSnapshots(readSnapshots().map(function (item) {
        return item.id === requestState.record.id ? { id: item.id, name: name, saved_at: item.saved_at, json: item.json } : item;
      }));
      renderSnapshots();
    } catch (error) { status(error.message, 'error'); }
  }
  function saveSnapshot() {
    openSnapshotNameDialog('save', null, document.activeElement);
  }
  function restoreSnapshot(record) {
    if (!snapshotNeedsColourData(record)) {
      try {
        status('Restoring “' + record.name + '”…', 'working');
        var summary = adapter().apply(JSON.parse(record.json), null);
        status('Restored ' + summary.selectedCells + ' selected cells and view settings.', 'success');
      } catch (error) {
        status(
          error && error.message
            ? error.message
            : 'This configuration uses a view that is unavailable here.',
          'error'
        );
      }
      return;
    }
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    var nonce = nextNonce();
    if (pending) return;
    startPending(nonce, 'apply');
    setBusy(true); status('Restoring “' + record.name + '”… Fetching gene colours.', 'working');
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
    openSnapshotNameDialog('rename', record, document.activeElement);
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
    var share = byId('cv-share-create');
    if (share) share.addEventListener('click', function () { sendShare('share_create'); });
    var upload = byId('coordviews_config_upload');
    if (upload) upload.addEventListener('change', beginUpload);
    byId('cv-snapshot-name-close').addEventListener('click', closeSnapshotNameDialog);
    byId('cv-snapshot-name-cancel').addEventListener('click', closeSnapshotNameDialog);
    byId('cv-snapshot-name-confirm').addEventListener('click', confirmSnapshotName);
    byId('cv-snapshot-name-input').addEventListener('keydown', function (event) {
      if (event.key === 'Enter') { event.preventDefault(); confirmSnapshotName(); }
    });
    byId('cv-snapshot-name-dialog').addEventListener('close', function () {
      var target = snapshotNameRequest && snapshotNameRequest.trigger;
      snapshotNameRequest = null;
      if (target && document.contains(target)) target.focus();
    });
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
      openShareFromUrl();
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
      Shiny.addCustomMessageHandler('coordviews_share_result', receiveShare);
    }
    window.setTimeout(openShareFromUrl, 0);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
