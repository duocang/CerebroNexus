/* Gene selectors search the whole gene list in the browser. The server sends
   the URL of the list, shared by every selector of the same genes; the list is
   fetched once and searched locally, so text typed before it arrives is
   searched when it does, and the best match of the typed text is highlighted.
   A list that arrives late cannot restore an old selection or inject the genes
   of a previous dataset. */
(function () {
  'use strict';

  var lists = {};

  function geneList(url) {
    if (!lists[url]) {
      lists[url] = window.fetch(url, {credentials: 'same-origin'}).then(function (response) {
        if (!response.ok) throw new Error('Gene list request failed: HTTP ' + response.status);
        return response.json();
      });
      lists[url].catch(function () { delete lists[url]; });
    }
    return lists[url];
  }

  function option(selectize, value) {
    var result = {};
    result[selectize.settings.valueField] = result[selectize.settings.labelField] = value;
    return result;
  }

  function ready(selectize) {
    return !!selectize && selectize._cerebroGeneChoicesReady === true;
  }

  function receive(element, message) {
    var selectize = element.selectize;
    var token = selectize._cerebroGeneChoicesToken = {};
    var selected = Object.prototype.hasOwnProperty.call(message, 'value')
      ? message.value : selectize.getValue();
    var values = Array.isArray(selected) ? selected : selected ? [selected] : [];
    selectize._cerebroGeneChoicesReady = false;
    // No server search, and none of the previous list's genes. clearOptions()
    // also clears the selection silently; it is set again at once.
    selectize.settings.load = null;
    selectize.clearOptions(true);
    values.forEach(function (value) { selectize.addOption(option(selectize, value)); });
    selectize.setValue(selected, true);
    // Report the selection, as Shiny's own update does.
    if (window.jQuery) window.jQuery(element).trigger('change');
    // Not returned: Shiny waits for receiveMessage before the next message.
    geneList(message.cerebro_gene_choices).then(function (genes) {
      if (token !== selectize._cerebroGeneChoicesToken) return;
      selectize.addOption(genes.map(function (gene) { return option(selectize, gene); }));
      selectize._cerebroGeneChoicesReady = true;
      // Search what was typed while the list was on its way.
      if (selectize.isFocused) selectize.refreshOptions(true);
    }, function (error) {
      if (window.console) window.console.warn(error);
    });
  }

  function install() {
    if (!window.Shiny || !Shiny.inputBindings) return;
    Shiny.inputBindings.getBindings().forEach(function (entry) {
      var binding = entry.binding;
      if (binding.name !== 'shiny.selectInput' || binding._cerebroGeneChoices) return;
      var original = binding.receiveMessage;
      binding.receiveMessage = function (element, message) {
        if (!element.selectize || !message || !message.cerebro_gene_choices) {
          return original.apply(this, arguments);
        }
        receive(element, message);
      };
      binding._cerebroGeneChoices = true;
    });
  }

  window.CerebroGeneSelector = {ready: ready};
  install();
  document.addEventListener('DOMContentLoaded', install);
  if (window.jQuery) window.jQuery(document).on('shiny:connected', install);
}());
