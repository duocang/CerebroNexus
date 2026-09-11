(function () {
  "use strict";

  var MOBILE_QUERY = "(max-width: 767px)";
  function ready(fn) {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", fn, { once: true });
    } else {
      fn();
    }
  }

  ready(function () {
    var body = document.body;
    var sidebar = document.querySelector(".main-sidebar");
    var toggle = document.querySelector(".main-header .sidebar-toggle");
    var close = document.getElementById("cerebro-nav-close");
    var scrim = document.getElementById("cerebro-nav-scrim");
    if (!body || !sidebar || !toggle || !close || !scrim) return;
    if (!sidebar.id) sidebar.id = "cerebro-primary-navigation";
    if (scrim.parentNode !== body) body.appendChild(scrim);

    function isMobile() { return window.matchMedia(MOBILE_QUERY).matches; }
    function isOpen() {
      return isMobile() && body.classList.contains("sidebar-open");
    }
    function syncSemantics() {
      var mobile = isMobile();
      var open = mobile && body.classList.contains("sidebar-open");
      toggle.setAttribute("aria-controls", sidebar.id);
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
      toggle.setAttribute("aria-label", open ? "Close navigation" : "Open navigation");
      scrim.setAttribute("aria-hidden", open ? "false" : "true");
      if (mobile) {
        sidebar.setAttribute("role", "dialog");
        sidebar.setAttribute("aria-label", "Primary navigation");
        sidebar.setAttribute("aria-modal", "true");
        sidebar.setAttribute("aria-hidden", open ? "false" : "true");
        sidebar.inert = !open;
      } else {
        ["role", "aria-label", "aria-modal", "aria-hidden"].forEach(function (name) {
          sidebar.removeAttribute(name);
        });
        sidebar.inert = false;
      }
    }
    function focusActiveDestination(navLink) {
      var moved = false;
      function move() {
        if (moved) return;
        var pane = document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
        if (!pane) return;
        var target = pane.querySelector("h1, h2, h3") || pane;
        target.classList.add("cerebro-page-focus-target");
        target.setAttribute("tabindex", "-1");
        target.focus({ preventScroll: true });
        moved = true;
      }
      if (window.jQuery) window.jQuery(navLink).one("shown.bs.tab", move);
      window.setTimeout(move, 80);
    }
    function setOpen(open, restoreFocus) {
      if (!isMobile()) return;
      if (open) {
        document.dispatchEvent(new CustomEvent("cerebro:overlay-opening", {
          detail: { owner: "nav" }
        }));
      }
      body.classList.toggle("sidebar-open", open);
      syncSemantics();
      if (open) {
        window.requestAnimationFrame(function () {
          if (isOpen()) close.focus();
        });
      } else if (restoreFocus !== false) {
        toggle.focus();
      }
    }

    document.addEventListener("click", function (event) {
      var target = event.target;
      if (!target || !target.closest) return;
      if (target.closest(".main-header .sidebar-toggle") && isMobile()) {
        event.preventDefault();
        event.stopImmediatePropagation();
        setOpen(!isOpen());
        return;
      }
      if (target.closest("#cerebro-nav-close") || target.closest("#cerebro-nav-scrim")) {
        event.preventDefault();
        setOpen(false);
        return;
      }
      var navLink = target.closest(".main-sidebar a[href]");
      if (navLink) {
        document.dispatchEvent(new CustomEvent("cerebro:overlay-opening", {
          detail: { owner: "nav" }
        }));
        if (isOpen()) setOpen(false, false);
        focusActiveDestination(navLink);
      }
    }, true);

    document.addEventListener("keydown", function (event) {
      if (!isOpen()) return;
      if (event.key === "Escape") {
        event.preventDefault();
        event.stopImmediatePropagation();
        setOpen(false);
        return;
      }
      if (event.key !== "Tab") return;
      var candidates = Array.prototype.filter.call(
        sidebar.querySelectorAll(
          "button:not([disabled]), a[href], input:not([disabled]), select:not([disabled])"
        ),
        function (element) { return element.getClientRects().length > 0; }
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
    }, true);

    document.addEventListener("cerebro:overlay-opening", function (event) {
      if (event.detail && event.detail.owner !== "nav" && isOpen()) {
        setOpen(false, false);
      }
    });
    window.addEventListener("resize", function () {
      if (!isMobile()) body.classList.remove("sidebar-open");
      syncSemantics();
    });
    syncSemantics();
  });

  ready(function () {
    if (!window.jQuery) return;
    function finish(element) {
      if (!element || !element.classList) return;
      window.clearTimeout(element.__cerebroWaitTimer);
      element.__cerebroWaitTimer = null;
      element.classList.remove("cerebro-output-waiting");
    }
    window.jQuery(document)
      .on("shiny:outputinvalidated.cerebroMotion", function (event) {
        var element = event.target;
        if (!element || !element.classList ||
            !element.classList.contains("shiny-bound-output")) return;
        if (!element.textContent.trim() && !element.children.length) return;
        finish(element);
        element.__cerebroWaitTimer = window.setTimeout(function () {
          element.classList.add("cerebro-output-waiting");
        }, 120);
      })
      .on("shiny:value.cerebroMotion shiny:error.cerebroMotion", function (event) {
        finish(event.target);
      });
  });

  ready(function () {
    var HANDLE = ".cerebro-selection-composition-drag";
    var HEADER = ".cerebro-selection-composition-head";
    var SLOT = ".cerebro-selection-composition-slot";
    var MOBILE = "(max-width: 700px)";
    var EDGE = 8;
    var drag = null;

    function isMobile() {
      return window.matchMedia(MOBILE).matches;
    }
    function frameOf(slot) {
      return slot && slot.parentElement;
    }
    function place(slot, left, top) {
      var frame = frameOf(slot);
      if (!frame || !slot.getClientRects().length || isMobile()) return;
      var frameRect = frame.getBoundingClientRect();
      var slotRect = slot.getBoundingClientRect();
      var maxLeft = Math.max(EDGE, frameRect.width - slotRect.width - EDGE);
      var maxTop = Math.max(EDGE, frameRect.height - slotRect.height - EDGE);
      slot.style.left = Math.max(EDGE, Math.min(maxLeft, left)) + "px";
      slot.style.top = Math.max(EDGE, Math.min(maxTop, top)) + "px";
      slot.style.bottom = "auto";
      slot.dataset.dragged = "true";
    }
    function pin(slot) {
      if (slot.dataset.dragged === "true") return;
      var frame = frameOf(slot);
      var frameRect = frame.getBoundingClientRect();
      var slotRect = slot.getBoundingClientRect();
      place(slot, slotRect.left - frameRect.left, slotRect.top - frameRect.top);
    }
    function clamp(slot) {
      if (!slot || slot.dataset.dragged !== "true") return;
      place(slot, parseFloat(slot.style.left) || EDGE,
        parseFloat(slot.style.top) || EDGE);
    }
    function clampAll() {
      Array.prototype.forEach.call(
        document.querySelectorAll(SLOT + '[data-dragged="true"]'),
        clamp
      );
    }
    function scheduleClamp() {
      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(clampAll);
      });
    }
    function finish(pointerId) {
      pointerId = typeof pointerId === "number"
        ? pointerId
        : pointerId && pointerId.pointerId;
      if (!drag || (pointerId != null && drag.pointerId !== pointerId)) return;
      if (drag.handle.hasPointerCapture &&
          drag.handle.hasPointerCapture(drag.pointerId)) {
        drag.handle.releasePointerCapture(drag.pointerId);
      }
      drag.slot.classList.remove("is-dragging");
      drag = null;
    }

    document.addEventListener("pointerdown", function (event) {
      var header = event.target.closest && event.target.closest(HEADER);
      if (!header || event.button !== 0 || isMobile()) return;
      var slot = header.closest(SLOT);
      if (!slot || !slot.getClientRects().length) return;
      pin(slot);
      var rect = slot.getBoundingClientRect();
      drag = {
        slot: slot,
        handle: header,
        pointerId: event.pointerId,
        offsetX: event.clientX - rect.left,
        offsetY: event.clientY - rect.top
      };
      if (header.setPointerCapture) header.setPointerCapture(event.pointerId);
      slot.classList.add("is-dragging");
      event.preventDefault();
    });
    document.addEventListener("pointermove", function (event) {
      if (!drag || drag.pointerId !== event.pointerId) return;
      var frameRect = frameOf(drag.slot).getBoundingClientRect();
      place(
        drag.slot,
        event.clientX - frameRect.left - drag.offsetX,
        event.clientY - frameRect.top - drag.offsetY
      );
      event.preventDefault();
    });
    document.addEventListener("pointerup", function (event) {
      finish(event.pointerId);
    });
    document.addEventListener("pointercancel", function (event) {
      finish(event.pointerId);
    });
    document.addEventListener("lostpointercapture", finish);
    window.addEventListener("blur", finish);
    document.addEventListener("keydown", function (event) {
      var handle = event.target.closest && event.target.closest(HANDLE);
      if (!handle || isMobile()) return;
      var moves = {
        ArrowLeft: [-1, 0], ArrowRight: [1, 0],
        ArrowUp: [0, -1], ArrowDown: [0, 1]
      };
      var move = moves[event.key];
      if (!move) return;
      var slot = handle.closest(SLOT);
      pin(slot);
      var step = event.shiftKey ? 1 : 10;
      place(
        slot,
        (parseFloat(slot.style.left) || EDGE) + move[0] * step,
        (parseFloat(slot.style.top) || EDGE) + move[1] * step
      );
      event.preventDefault();
      event.stopPropagation();
    });
    window.addEventListener("resize", scheduleClamp);
    if (window.jQuery) {
      window.jQuery(document).on(
        "shiny:value.cerebroCompositionDrag shown.bs.tab.cerebroCompositionDrag",
        scheduleClamp
      );
    }
  });
}());
