/* Keep choices-only initialization from overwriting a newer gene selection. */
(function () {
  'use strict';

  function install() {
    if (!window.Shiny || !Shiny.inputBindings) return;
    Shiny.inputBindings.getBindings().forEach(function (entry) {
      var binding = entry.binding;
      if (binding.name !== 'shiny.selectInput' || binding._cerebroGeneChoices) return;
      var receive = binding.receiveMessage;
      binding.receiveMessage = function (element, message) {
        var selectize = element.selectize;
        if (element.id !== 'expression_genes_input' || !selectize ||
            !message.url || Object.prototype.hasOwnProperty.call(message, 'value')) {
          return receive.apply(this, arguments);
        }
        var selected = selectize.items.slice();
        var options = selected.map(function (value) { return selectize.options[value]; });
        var update = Object.assign({}, message);
        // Shiny reads value again when the asynchronous choices request ends.
        // Use the then-current selection, including a deliberate user clear.
        Object.defineProperty(update, 'value', {
          enumerable: true,
          get: function () { return selectize.getValue(); }
        });
        var result = receive.call(this, element, update);
        // The binding clears options synchronously before starting its request.
        // Restore choices already made while this older message was in flight.
        options.forEach(function (option) { if (option) selectize.addOption(option); });
        selectize.setValue(selected);
        return result;
      };
      binding._cerebroGeneChoices = true;
    });
  }

  install();
  document.addEventListener('DOMContentLoaded', install);
  if (window.jQuery) window.jQuery(document).on('shiny:connected', install);
}());
