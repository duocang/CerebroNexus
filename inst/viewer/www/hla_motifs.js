/* HLA & TCR Motifs page helpers. The motif network itself is drawn and
   selected by the shared cell-view canvas (cell_views.js). */
(function () {
  'use strict';

  var associationObserver = null;
  var associationRequested = false;

  function requestAssociations() {
    if (associationRequested || !window.Shiny || !Shiny.setInputValue) return;
    associationRequested = true;
    Shiny.setInputValue('hla_associations_render_request', Date.now(), {
      priority: 'event'
    });
  }

  function observeAssociations() {
    var host = document.getElementById('hla_associations_mount');
    if (!host || host._renderObserved) return;
    host._renderObserved = true;
    associationObserver = new IntersectionObserver(function (entries) {
      if (entries.some(function (entry) { return entry.isIntersecting; })) {
        requestAssociations();
        associationObserver.disconnect();
      }
    });
    associationObserver.observe(host);
  }

  function init() {
    if (window.Shiny && Shiny.addCustomMessageHandler) observeAssociations();
  }

  window.addEventListener('cerebro:dataset-context', function (event) {
    var detail = event && event.detail || {};
    if (detail.phase !== 'pending') return;
    if (detail.changed || detail.selectionPending) {
      // The upload host stays mounted across datasets. Retire its old file.
      var upload = document.getElementById('hla_upload');
      if (upload) {
        var jq = window.jQuery;
        var uploader = jq && jq(upload).data('currentUploader');
        if (uploader && typeof uploader.abort === 'function') uploader.abort();
        if (jq) jq(upload).removeData('currentUploader').removeAttr('data-restore');
        upload.value = '';
        var group = upload.closest('.form-group');
        var filename = group && group.querySelector('input[type="text"]');
        var progress = group && group.querySelector('.progress');
        if (filename) filename.value = '';
        if (progress) progress.style.visibility = 'hidden';
        if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('hla_upload', null);
      }
      var provenance = document.getElementById('hla_upload_source_type');
      if (provenance && provenance.selectize) provenance.selectize.setValue('unknown');
    }
    ['hla_motif_network_composition', 'hla_selected_count'].forEach(function (id) {
      var root = document.getElementById(id);
      if (!root) return;
      if (window.Shiny && Shiny.unbindAll) Shiny.unbindAll(root);
      root.replaceChildren();
    });
  });

  if (window.jQuery) {
    // Node size: draw the slider's factor at once, without a frame from R
    // (frames carry the base sizes and the factor as data.point_scale).
    window.jQuery(document).on('change.hlaNodeScale', '#hla_node_scale', function () {
      var api = window.cerebroCellViews;
      if (api && api.scalePointSizes) api.scalePointSizes('hla_motif_network', Number(this.value));
    });
    window.jQuery(document).on('shiny:connected', init);
  } else {
    document.addEventListener('shiny:connected', init, { once: true });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
