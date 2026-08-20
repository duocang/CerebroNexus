/* Portable Linked views configuration dialog and Shiny transport. */
(function () {
  'use strict';

  var pending = null;
  var sequence = 0;
  var lastFocus = null;
  var exportReady = false;
  var exportBusy = false;

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
      pending = { nonce: nonce, action: action };
      setBusy(true);
      status(action === 'copy' ? 'Preparing JSON to copy…' : 'Preparing your download…');
      Shiny.setInputValue('coordviews_config_request', {
        nonce: nonce,
        action: action,
        config: state.capture()
      }, { priority: 'event' });
    } catch (error) {
      pending = null;
      setBusy(false);
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
  function receive(result) {
    if (!result || !pending || String(result.nonce) !== pending.nonce ||
      result.action !== pending.action) return;
    var action = pending.action;
    pending = null;
    setBusy(false);
    if (action === 'apply') {
      var upload = byId('coordviews_config_upload');
      if (upload) upload.value = '';
      setUploadLoading(false);
    }
    if (!result.ok) {
      status(result.message || 'The configuration could not be opened.', 'error');
      return;
    }
    if (action === 'copy' && result.action === 'copy') finishCopy(result);
    else if (action === 'download' && result.action === 'download') finishDownload(result);
    else if (action === 'apply' && result.action === 'apply') finishApply(result);
  }
  function beginUpload() {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var nonce = nextNonce();
    pending = { nonce: nonce, action: 'apply' };
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
