(function () {
  'use strict';

  var records = [];
  var pending = null;
  var shinyBound = false;
  function byId(id) { return document.getElementById(id); }
  function nonce() {
    return Date.now().toString(36) + '-' + Math.random().toString(36).slice(2);
  }
  function status(message, kind) {
    var node = byId('viewer-admin-status');
    if (!node) return;
    node.textContent = message || '';
    node.className = 'viewer-admin-status' + (kind ? ' is-' + kind : '');
  }
  function appBasePath() {
    var path = window.location.pathname.replace(/\/admin\/?$/, '/');
    return path || '/';
  }
  function shareUrl(token) {
    var url = new URL(appBasePath(), window.location.origin);
    url.searchParams.set('linked_view', token);
    return url.toString();
  }
  function fallbackCopy(text) {
    var input = null;
    var copied = false;
    try {
      input = document.createElement('textarea');
      input.value = text;
      input.setAttribute('readonly', 'readonly');
      input.style.cssText = 'position:fixed;left:-9999px;top:0;opacity:0';
      document.body.appendChild(input);
      input.focus(); input.select();
      copied = document.execCommand('copy');
    } catch (ignore) {
      copied = false;
    } finally {
      if (input && input.parentNode) input.parentNode.removeChild(input);
    }
    return Promise.resolve(copied);
  }
  function copyText(text) {
    return fallbackCopy(text).then(function (copied) {
      if (copied || !navigator.clipboard || !navigator.clipboard.writeText) return copied;
      return new Promise(function (resolve) {
        var settled = false;
        function finish(result) {
          if (settled) return;
          settled = true;
          window.clearTimeout(timer);
          resolve(!!result);
        }
        var timer = window.setTimeout(function () { finish(false); }, 500);
        try {
          Promise.resolve(navigator.clipboard.writeText(text)).then(
            function () { finish(true); },
            function () { finish(false); }
          );
        } catch (ignore) { finish(false); }
      });
    }).catch(function () { return false; });
  }
  function formatDate(value) {
    var date = new Date(value);
    return isNaN(date.getTime()) ? value : date.toLocaleString();
  }
  function button(label, action, token) {
    var item = document.createElement('button');
    item.type = 'button'; item.className = 'viewer-admin-action';
    item.dataset.action = action; item.dataset.token = token; item.textContent = label;
    return item;
  }
  function render() {
    var host = byId('viewer-admin-share-list');
    if (!host) return;
    host.replaceChildren();
    if (!records.length) {
      var empty = document.createElement('p');
      empty.className = 'viewer-admin-empty';
      empty.textContent = 'No active share links.';
      host.appendChild(empty); return;
    }
    var table = document.createElement('table');
    table.className = 'viewer-admin-table';
    var head = document.createElement('thead');
    head.innerHTML = '<tr><th>Cell population</th><th>Created</th><th>Expires</th><th>Created by</th><th><span class="sr-only">Actions</span></th></tr>';
    table.appendChild(head);
    var body = document.createElement('tbody');
    records.forEach(function (record) {
      var row = document.createElement('tr');
      var dataset = document.createElement('td');
      var name = document.createElement('strong');
      name.textContent = record.dataset_label || 'Unnamed dataset';
      var fingerprint = document.createElement('small');
      fingerprint.textContent = String(record.fingerprint || '').slice(0, 16) + '…';
      dataset.appendChild(name); dataset.appendChild(fingerprint); row.appendChild(dataset);
      [formatDate(record.created_at), formatDate(record.expires_at), record.creator || 'Administrator'].forEach(function (text) {
        var cell = document.createElement('td'); cell.textContent = text; row.appendChild(cell);
      });
      var actions = document.createElement('td'); actions.className = 'viewer-admin-actions';
      actions.appendChild(button('Copy link', 'copy', record.token));
      actions.appendChild(button('Revoke', 'revoke', record.token));
      row.appendChild(actions); body.appendChild(row);
    });
    table.appendChild(body); host.appendChild(table);
  }
  function send(action, token) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue || pending) return;
    pending = { nonce: nonce(), action: action, token: token || '' };
    status(action === 'revoke' ? 'Revoking link…' : 'Refreshing links…', 'working');
    Shiny.setInputValue('viewer_admin_request', pending, { priority: 'event' });
  }
  function receive(result) {
    if (!result) return;
    if (pending && result.nonce && result.nonce !== pending.nonce) return;
    pending = null;
    if (!result.ok) {
      status(result.message || 'The Admin request failed.', 'error'); return;
    }
    if (result.action === 'list') {
      records = Array.isArray(result.records) ? result.records : [];
      render(); status(records.length ? records.length + ' active share link' + (records.length === 1 ? '.' : 's.') : 'No active share links.');
    } else if (result.action === 'revoke') {
      records = records.filter(function (record) { return record.token !== result.token; });
      render(); status('Share link revoked.', 'success');
    }
  }
  function connectShiny() {
    if (shinyBound || typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
    shinyBound = true;
    Shiny.addCustomMessageHandler('viewer_admin_result', receive);
    Shiny.addCustomMessageHandler('viewer_admin_access', function (result) {
      if (result && result.allowed === false) {
        window.history.replaceState(
          {},
          '',
          appBasePath() + window.location.search + window.location.hash
        );
      }
    });
  }
  function boot() {
    var refresh = byId('viewer-admin-refresh');
    if (refresh) refresh.addEventListener('click', function () { send('list'); });
    var list = byId('viewer-admin-share-list');
    if (list) list.addEventListener('click', function (event) {
      var target = event.target.closest('[data-action]');
      if (!target) return;
      if (target.dataset.action === 'copy') {
        target.disabled = true; target.textContent = 'Copying…';
        copyText(shareUrl(target.dataset.token)).then(function (ok) {
          target.textContent = ok ? 'Copied ✓' : 'Copy failed';
          status(ok ? 'Share link copied.' : 'Clipboard access was blocked.', ok ? 'success' : 'error');
          window.setTimeout(function () { target.disabled = false; target.textContent = 'Copy link'; }, 1400);
        });
      } else if (target.dataset.action === 'revoke') {
        send('revoke', target.dataset.token);
      }
    });
    if (window.jQuery) window.jQuery(document).one('shiny:connected', connectShiny);
    else document.addEventListener('shiny:connected', connectShiny, { once: true });
    connectShiny();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot);
  else boot();
})();
