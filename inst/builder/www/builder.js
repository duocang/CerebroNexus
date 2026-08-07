/* Cerebro Dataset Builder: lightweight client interactions.
 *
 * Layout belongs to CSS. The client only forwards semantic data attributes to
 * Shiny and marks the detail fragment as ready after Shiny has bound its new
 * inputs. There are no scroll, resize or geometry observers here.
 */
(function () {
  "use strict";

  function send(name, value) {
    if (window.Shiny) {
      window.Shiny.setInputValue(name, value, { priority: "event" });
    }
  }

  document.addEventListener("click", function (event) {
    var target = event.target;
    var removeDataset = target.closest(".builder-drop");
    if (removeDataset) {
      event.preventDefault();
      event.stopPropagation();
      send("drop_ds", removeDataset.dataset.ds);
      return;
    }

    var pick = target.closest(".builder-pick");
    if (pick) {
      send("pick", pick.dataset.ds);
      return;
    }

    var browse = target.closest(".builder-browse");
    if (browse) {
      send("browse_to", browse.dataset.path);
      return;
    }

    var choose = target.closest(".builder-choose");
    if (choose) {
      send("choose_file", choose.dataset.path);
      return;
    }

    var example = target.closest(".example-btn");
    if (example) {
      example.classList.add("is-taken");
      send("use_example", example.dataset.ex);
      return;
    }

    var removeTable = target.closest(".builder-table-drop");
    if (removeTable) {
      send("drop_table", removeTable.dataset.name);
    }
  });

  document.addEventListener("change", function (event) {
    var analysis = event.target.closest(".builder-analysis");
    if (analysis) {
      var detail = document.getElementById("detail");
      var selected = Array.from(
        detail.querySelectorAll(".builder-analysis:checked")
      ).map(function (box) {
        return box.value;
      });
      send("analyses", { ds: analysis.dataset.ds, v: selected });
      return;
    }

    var swatch = event.target.closest(".swatch-input");
    if (swatch) {
      send("swatch_change", {
        ds: swatch.dataset.ds,
        group: swatch.dataset.group,
        level: swatch.dataset.level,
        value: swatch.value,
      });
    }
  });

  function announceDetail() {
    var flag = document.querySelector("#detail .pane-flag[data-ds]");
    if (!flag) return;
    window.setTimeout(function () {
      send("rendered_for", flag.dataset.ds);
    }, 0);
  }

  document.addEventListener("shiny:connected", function () {
    var detail = document.getElementById("detail");
    if (!detail) return;
    new MutationObserver(announceDetail).observe(detail, {
      childList: true,
      subtree: false,
    });
    announceDetail();
  });

  document.addEventListener("shiny:sessioninitialized", function () {
    window.Shiny.addCustomMessageHandler(
      "builder_used_examples",
      function (message) {
        var used = new Set((message && message.ids) || []);
        document.querySelectorAll(".example-btn[data-ex]").forEach(function (el) {
          el.classList.toggle("is-taken", used.has(el.dataset.ex));
        });
      }
    );
  });
})();
