(function () {
  'use strict';

  var MOBILE_QUERY = '(max-width: 767px)';

  function ready(fn) {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', fn, { once: true });
    } else {
      fn();
    }
  }

  ready(function () {
    var body = document.body;
    var sidebar = document.querySelector('.main-sidebar');
    var toggle = document.querySelector('.main-header .sidebar-toggle');
    var close = document.getElementById('cerebro-nav-close');
    var scrim = document.getElementById('cerebro-nav-scrim');
    if (!body || !sidebar || !toggle || !close || !scrim) return;

    // The backdrop must sit beside the app shell: transformed AdminLTE content
    // would otherwise turn its fixed positioning into content-relative geometry.
    if (scrim.parentNode !== body) body.appendChild(scrim);

    var isMobile = function () {
      return window.matchMedia(MOBILE_QUERY).matches;
    };
    var isOpen = function () {
      return isMobile() && body.classList.contains('sidebar-open');
    };

    function syncSemantics() {
      var mobile = isMobile();
      var open = mobile && body.classList.contains('sidebar-open');
      toggle.setAttribute('aria-controls', sidebar.id);
      toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
      toggle.setAttribute('aria-label', open ? 'Close navigation' : 'Open navigation');
      scrim.setAttribute('aria-hidden', open ? 'false' : 'true');
      if (mobile) {
        sidebar.setAttribute('role', 'dialog');
        sidebar.setAttribute('aria-modal', 'true');
        sidebar.setAttribute('aria-hidden', open ? 'false' : 'true');
      } else {
        sidebar.removeAttribute('role');
        sidebar.removeAttribute('aria-modal');
        sidebar.removeAttribute('aria-hidden');
      }
    }

    function setOpen(open, restoreFocus) {
      if (!isMobile()) return;
      if (open) {
        document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
          detail: { owner: 'nav' }
        }));
      }
      body.classList.toggle('sidebar-open', open);
      syncSemantics();
      if (open) {
        window.requestAnimationFrame(function () {
          if (isOpen()) close.focus();
        });
      } else if (restoreFocus !== false) {
        toggle.focus();
      }
    }

    // Capture the mobile toggle before AdminLTE's push-menu handler can shift
    // the content canvas. Desktop keeps the framework's native behaviour.
    document.addEventListener('click', function (event) {
      var target = event.target;
      if (!target || !target.closest) return;
      var toggleHit = target.closest('.main-header .sidebar-toggle');
      if (toggleHit && isMobile()) {
        event.preventDefault();
        event.stopImmediatePropagation();
        setOpen(!isOpen());
        return;
      }
      if (target.closest('#cerebro-nav-close')) {
        event.preventDefault();
        setOpen(false);
        return;
      }
      if (target.closest('#cerebro-nav-scrim')) {
        event.preventDefault();
        setOpen(false);
        return;
      }
      if (isOpen() && target.closest('.main-sidebar a[href]')) {
        setOpen(false, false);
      }
    }, true);

    document.addEventListener('keydown', function (event) {
      if (!isOpen()) return;
      if (event.key === 'Escape') {
        event.preventDefault();
        setOpen(false);
        return;
      }
      if (event.key !== 'Tab') return;
      var candidates = Array.prototype.filter.call(
        sidebar.querySelectorAll('button:not([disabled]), a[href], input:not([disabled]), select:not([disabled])'),
        function (el) { return el.getClientRects().length > 0; }
      );
      if (!candidates.length) return;
      var first = candidates[0];
      var last = candidates[candidates.length - 1];
      if (event.shiftKey && document.activeElement === first) {
        event.preventDefault();
        last.focus();
      } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault();
        first.focus();
      }
    });

    document.addEventListener('cerebro:overlay-opening', function (event) {
      if (event.detail && event.detail.owner !== 'nav' && isOpen()) {
        setOpen(false, false);
      }
    });

    window.addEventListener('resize', function () {
      if (!isMobile()) body.classList.remove('sidebar-open');
      syncSemantics();
    });
    syncSemantics();
  });
})();
