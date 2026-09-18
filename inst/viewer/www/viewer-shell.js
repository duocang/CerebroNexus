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
    var busyOutputs = new Set();
    var updateAnnounced = false;
    var statusClearTimer = null;
    function announce(message) {
      var status = document.getElementById("cerebro-update-status");
      if (!status) return;
      window.clearTimeout(statusClearTimer);
      status.textContent = message;
      if (message === "Content updated") {
        statusClearTimer = window.setTimeout(function () {
          status.textContent = "";
        }, 1500);
      }
    }
    function pruneBusyOutputs() {
      busyOutputs.forEach(function (element) {
        if (element.isConnected && element.getClientRects().length) return;
        window.clearTimeout(element.__cerebroWaitTimer);
        element.__cerebroWaitTimer = null;
        element.classList.remove("cerebro-output-waiting");
        element.removeAttribute("aria-busy");
        busyOutputs.delete(element);
      });
    }
    function finish(element) {
      if (!element || !element.classList) return;
      window.clearTimeout(element.__cerebroWaitTimer);
      element.__cerebroWaitTimer = null;
      element.classList.remove("cerebro-output-waiting");
      element.removeAttribute("aria-busy");
      busyOutputs.delete(element);
      pruneBusyOutputs();
      if (updateAnnounced && !busyOutputs.size) {
        updateAnnounced = false;
        announce("Content updated");
      }
    }
    window.jQuery(document)
      .on("shiny:outputinvalidated.cerebroMotion", function (event) {
        var element = event.target;
        if (!element || !element.classList ||
            !element.classList.contains("shiny-bound-output")) return;
        if (!element.getClientRects().length) {
          finish(element);
          return;
        }
        window.clearTimeout(element.__cerebroWaitTimer);
        element.classList.remove("cerebro-output-waiting");
        element.setAttribute("aria-busy", "true");
        busyOutputs.add(element);
        element.__cerebroWaitTimer = window.setTimeout(function () {
          if (element.getAttribute("aria-busy") !== "true") return;
          if (element.textContent.trim() || element.children.length) {
            element.classList.add("cerebro-output-waiting");
          }
          if (!updateAnnounced) {
            updateAnnounced = true;
            announce("Updating content");
          }
        }, 120);
      })
      .on("shiny:value.cerebroMotion shiny:error.cerebroMotion", function (event) {
        finish(event.target);
      })
      .on("shown.bs.tab.cerebroMotion", function () {
        pruneBusyOutputs();
        if (updateAnnounced && !busyOutputs.size) {
          updateAnnounced = false;
          announce("Content updated");
        }
      });
  });

  ready(function () {
    if (!window.jQuery) return;
    var loader = document.getElementById("cerebro-page-loader");
    var detail = document.getElementById("cerebro-page-loader-detail");
    if (!loader || !detail) return;

    var activePane = null;
    var slowTimer = null;
    var hideTimer = null;
    var settleToken = 0;
    var shinyBusy = false;
    var canvasTabs = new Set([
      "overview",
      "coordinated_views",
      "geneExpression",
      "trajectory",
      "spatial",
      "trekker",
      "hla_tcr_motifs"
    ]);

    function paneName(pane) {
      return pane && pane.id ? pane.id.replace(/^shiny-tab-/, "") : "";
    }
    function currentPane() {
      return document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
    }
    function setBusy(pane, busy) {
      if (!pane) return;
      if (busy) pane.setAttribute("aria-busy", "true");
      else pane.removeAttribute("aria-busy");
    }
    function cancelScheduledFinish() {
      settleToken += 1;
    }
    function finish(pane) {
      if (!pane) return;
      cancelScheduledFinish();
      pane.dataset.cerebroPageReady = "true";
      setBusy(pane, false);
      if (pane !== activePane) return;
      window.clearTimeout(slowTimer);
      window.clearTimeout(hideTimer);
      loader.classList.remove("is-visible");
      hideTimer = window.setTimeout(function () {
        if (!loader.classList.contains("is-visible")) loader.hidden = true;
      }, 150);
      activePane = null;
    }
    function begin(pane) {
      if (!pane) return;
      cancelScheduledFinish();
      window.clearTimeout(slowTimer);
      window.clearTimeout(hideTimer);
      if (pane.dataset.cerebroPageReady === "true") {
        if (activePane && activePane !== pane) setBusy(activePane, false);
        activePane = pane;
        finish(pane);
        return;
      }
      if (activePane && activePane !== pane) setBusy(activePane, false);
      activePane = pane;
      setBusy(pane, true);
      detail.textContent = "Preparing data and visualisation…";
      loader.hidden = false;
      loader.classList.add("is-visible");
      slowTimer = window.setTimeout(function () {
        if (pane === activePane) {
          detail.textContent = "Still preparing this view…";
        }
      }, 15000);
    }
    function scheduleFinish(pane) {
      if (!pane || pane !== currentPane() ||
          canvasTabs.has(paneName(pane))) return;
      var token = ++settleToken;
      // Shiny can emit several values for one page. Wait for a short quiet
      // period, then cross two browser paint boundaries, so the overlay never
      // disappears between the first small output and the final chart/table.
      window.setTimeout(function () {
        if (token !== settleToken || shinyBusy || pane !== currentPane()) return;
        window.requestAnimationFrame(function () {
          window.requestAnimationFrame(function () {
            if (token !== settleToken || shinyBusy || pane !== currentPane()) return;
            finish(pane);
          });
        });
      }, 180);
    }
    function resetPages() {
      document.querySelectorAll('.tab-pane[id^="shiny-tab-"]').forEach(
        function (pane) {
          delete pane.dataset.cerebroPageReady;
          pane.removeAttribute("aria-busy");
        }
      );
      begin(currentPane());
    }
    function paneFor(element) {
      return element && element.closest
        ? element.closest('.tab-pane[id^="shiny-tab-"]')
        : null;
    }

    window.jQuery(document)
      .on("shown.bs.tab.cerebroPageLoader", function () {
        var pane = currentPane();
        begin(pane);
        if (!shinyBusy) scheduleFinish(pane);
      })
      .on("shiny:inputchanged.cerebroPageLoader", function (event) {
        if (event.name === "crb_file_selector") resetPages();
      })
      .on("shiny:busy.cerebroPageLoader", function () {
        shinyBusy = true;
        cancelScheduledFinish();
      })
      .on("shiny:idle.cerebroPageLoader", function () {
        shinyBusy = false;
        var pane = currentPane();
        if (!pane || canvasTabs.has(paneName(pane))) return;
        scheduleFinish(pane);
      })
      .on("shiny:value.cerebroPageLoader", function (event) {
        var pane = paneFor(event.target);
        if (pane && pane === currentPane() && !canvasTabs.has(paneName(pane))) {
          cancelScheduledFinish();
          if (!shinyBusy) scheduleFinish(pane);
        }
      })
      .on("shiny:error.cerebroPageLoader", function (event) {
        var pane = paneFor(event.target);
        if (pane && pane === currentPane()) {
          cancelScheduledFinish();
          if (!shinyBusy) scheduleFinish(pane);
        }
      })
      .on("shiny:connected.cerebroPageLoader", function () {
        begin(currentPane());
      });

    window.addEventListener("cerebro:cell-view-ready", function (event) {
      if (!event.detail || !event.detail.painted) return;
      var id = event.detail && event.detail.id;
      if (id && event.detail.renderToken != null && window.Shiny &&
          Shiny.setInputValue) {
        Shiny.setInputValue(id + "_render_complete", {
          render_token: event.detail.renderToken,
          nonce: Date.now()
        }, { priority: "event" });
      }
      var host = id && document.getElementById(id + "_cell_view_host");
      var pane = paneFor(host);
      if (pane && pane === currentPane()) finish(pane);
    });
    window.addEventListener("cerebro:linkedviews-ready", function (event) {
      if (!event.detail || !event.detail.ready || !event.detail.painted) return;
      var pane = document.getElementById("shiny-tab-coordinated_views");
      if (pane && pane === currentPane()) finish(pane);
    });

    var content = document.getElementById("main-content");
    if (content && window.MutationObserver) {
      new MutationObserver(function (records) {
        var pane = activePane;
        if (!pane || shinyBusy || canvasTabs.has(paneName(pane))) return;
        var changed = records.some(function (record) {
          return pane.contains(record.target) && !loader.contains(record.target);
        });
        if (changed) scheduleFinish(pane);
      }).observe(content, {
        subtree: true,
        childList: true,
        characterData: true,
        attributes: true
      });
    }
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
