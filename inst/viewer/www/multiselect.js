/* Global Selectize multi-select polish: readable empty state and full labels. */
(function () {
  "use strict";

  var placeholder = "Select…";

  function minimumEmptyWidth() {
    var more = document.getElementById("cv-more-btn");
    return more ? Math.ceil(more.getBoundingClientRect().width) : 140;
  }

  function emptyWidth(select, instance) {
    var control = instance.$control && instance.$control[0];
    var style = window.getComputedStyle(control || select);
    var canvas = emptyWidth.canvas || (emptyWidth.canvas = document.createElement("canvas"));
    var context = canvas.getContext("2d");
    context.font = style.font || [style.fontSize, style.fontFamily].join(" ");
    var longest = Array.prototype.reduce.call(select.options, function (width, option) {
      return Math.max(width, context.measureText(option.textContent || "").width);
    }, 0);
    return Math.min(
      window.innerWidth - 32,
      Math.max(minimumEmptyWidth(), Math.ceil(longest + 42))
    );
  }

  function sizeEmptyControl(select, instance) {
    var empty = !instance.items.length;
    instance.$wrapper.toggleClass("cerebro-multiselect-empty", empty);
    instance.$wrapper.css("width", empty ? emptyWidth(select, instance) + "px" : "");
  }

  function enhance(select) {
    if (!select || !select.multiple) return;
    select.setAttribute("data-placeholder", placeholder);
    var instance = select.selectize;
    if (!instance) return;
    instance.settings.placeholder = placeholder;
    if (instance.$control_input && instance.$control_input.length) {
      instance.$control_input.attr("placeholder", placeholder);
    }
    instance.updatePlaceholder();
    instance.$wrapper.addClass("cerebro-multiselect");
    sizeEmptyControl(select, instance);
    if (!select.dataset.cerebroMultiSelectReady) {
      instance.on("change", function () { sizeEmptyControl(select, instance); });
      select.dataset.cerebroMultiSelectReady = "true";
    }
  }

  function enhanceAll(root) {
    var scope = root && root.querySelectorAll ? root : document;
    Array.prototype.forEach.call(
      scope.querySelectorAll("select[multiple]"),
      enhance
    );
    if (scope.matches && scope.matches("select[multiple]")) enhance(scope);
  }

  function schedule(root) {
    window.setTimeout(function () { enhanceAll(root); }, 0);
  }

  document.addEventListener("DOMContentLoaded", function () { schedule(document); });
  document.addEventListener("shiny:connected", function () { schedule(document); });
  document.addEventListener("shiny:value", function (event) { schedule(event.target); });
  new MutationObserver(function (mutations) {
    mutations.forEach(function (mutation) {
      Array.prototype.forEach.call(mutation.addedNodes, schedule);
    });
  }).observe(document.documentElement, { childList: true, subtree: true });
}());
