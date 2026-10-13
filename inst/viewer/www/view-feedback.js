/* Presentation only: control requests acknowledge existing painted tokens.
 * No new Shiny inputs, data reads, render scheduling, or readiness signals. */
(function () {
  'use strict';
  var specs = {
    overview_projection: ['overview', 'Overview', /^overview_projection_(to_display|point_color|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$/],
    expression_projection: ['geneExpression', 'Gene expression', /^expression_projection_(to_display|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$/],
    trajectory_projection: ['trajectory', 'Trajectory', /^trajectory_(selected_method|selected_name|point_color|point_size|point_opacity|projection_group_labels|projection_point_border|projection_keep_square|percentage_cells_to_show|projection_group_filter_.+)$/],
    spatial_projection: ['spatial', 'Spatial', /^spatial_projection_(to_display|plot_type|point_color|point_size|point_opacity|group_labels|point_border|keep_square|percentage_cells_to_show|feature_to_display|coexpr_[rgb]|show_region_outlines|background_image|group_filter_.+)$/],
    hla_motif_network: ['hla_tcr_motifs', 'HLA / TCR motifs', /^hla_(chain|scope|color_by|color_allele|pair_allele_i|pair_allele_ii|min_nodes|by_v|show_isolated|node_scale|legend_mode|group_filter_.+)$/],
    linked: ['coordinated_views', 'Linked views', /^$/]
  };
  var states = {}, installed = false;
  var layoutTimers = {}, dismissedLayout = {};
  var layoutControls = {
    overview_projection: ['overview_projection_more_button', 'overview_projection_keep_square'],
    expression_projection: ['expression_projection_more_button', 'expression_projection_keep_square'],
    linked: ['cv-more-btn', 'cv-square-plots']
  };
  function layoutDismissed(id) {
    try { return dismissedLayout[id] || sessionStorage.getItem('cerebro-square-hint:' + id) === 'dismissed'; }
    catch (_) { return !!dismissedLayout[id]; }
  }
  function dismissLayout(id) {
    dismissedLayout[id] = true;
    try { sessionStorage.setItem('cerebro-square-hint:' + id, 'dismissed'); } catch (_) {}
    draw(id);
  }
  function openSquareSetting(id) {
    var controls = layoutControls[id], button = controls && document.getElementById(controls[0]);
    if (!button) return;
    if (button.getAttribute('aria-expanded') !== 'true') button.click();
    var attempts = 0;
    function focusSetting() {
      var input = document.getElementById(controls[1]);
      if (input && input.getClientRects().length) {
        input.scrollIntoView({ block: 'nearest', behavior: 'auto' });
        input.focus({ preventScroll: true });
        var label = input.closest('.checkbox') || input.parentElement;
        label.classList.add('cerebro-setting-highlight');
        setTimeout(function () { label.classList.remove('cerebro-setting-highlight'); }, 2000);
      } else if (++attempts < 40) setTimeout(focusSetting, 50);
    }
    setTimeout(focusSetting, 80);
  }
  function normal(value) {
    return JSON.stringify((value == null ? [] : Array.isArray(value) ? value : [value]).map(String).sort());
  }
  function state(id) {
    return states[id] || (states[id] = { pending: {}, frames: new Map(), painted: null,
      ready: false, selection: null, auxiliary: '', operation: '', terminal: '' });
  }
  function fresh(message) {
    return window.CerebroDatasetContext && window.CerebroDatasetContext.accepts(message);
  }
  function row(id) {
    var pane = document.getElementById('shiny-tab-' + specs[id][0]);
    if (!pane) return null;
    var el = pane.querySelector('.cerebro-page-feedback');
    if (!el) {
      el = document.createElement('div');
      el.className = 'cerebro-view-feedback cerebro-page-feedback';
      el.setAttribute('role', 'status');
      el.setAttribute('aria-live', 'polite');
      var heading = pane.querySelector('.cerebro-viz-page-heading');
      if (heading) heading.insertAdjacentElement('afterend', el);
      else pane.insertBefore(el, pane.firstChild);
    }
    return el;
  }
  function mark(key, pending) {
    var el = document.getElementById(key);
    var group = el && el.closest('.form-group, .cv-filt');
    if (group) group.classList.toggle('cerebro-control-pending', pending);
  }
  function draw(id) {
    var s = state(id), el = row(id);
    if (!el) return;
    var pending = Object.keys(s.pending);
    var text = pending.length ? 'Updating: ' + pending.map(function (key) {
      return s.pending[key].label;
    }).join(' · ') : s.terminal || (s.ready ? s.empty ? 'No cells match these filters' : '' : 'Loading ' + specs[id][1] + '…');
    // Settled selection counts already belong to the Active cohort toolbar.
    text = [text, s.operation, s.expression, s.selection, s.auxiliary].filter(Boolean).join(' · ');
    el.textContent = '';
    var status = document.createElement('span');
    status.className = 'cerebro-feedback-text'; status.textContent = text;
    el.appendChild(status);
    var pane = el.closest('.tab-pane');
    var showHint = layoutControls[id] && s.wideLayout && s.ready && !s.empty && !s.terminal &&
        !pending.length && !layoutDismissed(id) && pane && pane.classList.contains('active');
    if (showHint) {
      var hint = document.createElement('span');
      hint.className = 'cerebro-layout-hint';
      hint.innerHTML = '<span class="cerebro-layout-copy"><i class="fas fa-expand" aria-hidden="true"></i> Wide UMAP</span>' +
        '<button type="button" class="cerebro-layout-settings" data-layout-settings="' + id +
        '" title="Open Settings and find Keep plots square">Square plot settings</button>' +
        '<button type="button" class="cerebro-layout-dismiss" data-layout-dismiss="' + id +
        '" aria-label="Dismiss square plot suggestion for this session">×</button>';
      el.appendChild(hint);
    }
    el.hidden = !text && !showHint;
    el.classList.toggle('is-pending', !s.terminal && (pending.length > 0 || !s.ready));
    el.classList.toggle('is-terminal', !!s.terminal);
    el.classList.toggle('is-error', !!s.terminal && s.terminalKind !== 'empty');
    // A terminal result is explicitly not a painted/ready acknowledgement.
    var host = document.getElementById(id + '_cell_view_host');
    if (host) host.classList.toggle('cerebro-view-terminal', !!s.terminal);
  }
  function settle(id) {
    var s = state(id), controls = s.frames.get(s.painted);
    if (!controls) return;
    Object.keys(s.pending).forEach(function (key) {
      if (Object.prototype.hasOwnProperty.call(controls, key) &&
          normal(controls[key]) === s.pending[key].value) {
        delete s.pending[key]; mark(key, false);
      }
    });
  }
  function inputValue(el) {
    if (el.selectize) return el.selectize.getValue();
    if (el.type === 'checkbox') return el.checked;
    if (el.multiple) return Array.from(el.selectedOptions).map(function (o) { return o.value; });
    return el.value;
  }
  function changed(event) {
    var key = event.name || event.target.id;
    if (/^(coordviews_gene(?:_[rgb])?|coordviews_expression_mode|cv-pick-color)$/.test(key)) {
      linkedExpressionChange(key);
      return;
    }
    var id = Object.keys(specs).find(function (id) { return specs[id][2].test(key); });
    if (!id || !window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
    var el = document.getElementById(key), group = el && el.closest('.form-group, .cv-filt');
    // Ignore binding/default updates. Selectize, sliders and keyboard input all
    // retain focus within their form group during user-generated changes.
    if (!el || !group || !group.contains(document.activeElement)) return;
    var value = event.name ? event.value : inputValue(el);
    var label = group.querySelector('.cv-filt-btn') || group.querySelector('label');
    var shown = /group_filter_/.test(key) ? (Array.isArray(value) ? value.length : value ? 1 : 0) + ' selected' : el.selectize ? el.selectize.items.map(function (v) {
      var option = el.selectize.options[v];
      return option && option[el.selectize.settings.labelField] || v;
    }).join(', ') : el.tagName === 'SELECT' ? Array.from(el.selectedOptions).map(function (o) { return o.textContent; }).join(', ') : String(value);
    var s = state(id);
    s.pending[key] = { value: normal(value), label: ((label && label.textContent.trim()) || 'Parameter') + ' ' + shown };
    mark(key, true); settle(id); draw(id);
  }
  function linkedControls(rgb) {
    var ids = rgb ? ['coordviews_gene_r', 'coordviews_gene_g', 'coordviews_gene_b'] : ['coordviews_gene'];
    var controls = {};
    ids.forEach(function (id) { var el = document.getElementById(id); controls[id] = el ? inputValue(el) : []; });
    if (!rgb) {
      var mode = document.getElementById('cv-pick-color');
      controls.coordviews_expression_mode = mode && inputValue(mode) === '__gene_panels__' ? 'panels' : 'mean';
    }
    return controls;
  }
  function linkedExpressionChange(key) {
    if (!window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
    var picker = document.getElementById('cv-pick-color');
    var mode = picker && inputValue(picker), rgb = mode === '__rgb__';
    var s = state('linked');
    if (['__gene__', '__gene_panels__', '__rgb__'].indexOf(mode) < 0) {
      s.expression = ''; s.expressionRequest = null; draw('linked'); return;
    }
    // Switching back to an already loaded mode need not send a server request.
    // Only the actual gene/Shiny mode input owns a new pending operation.
    if (key === 'cv-pick-color') {
      s.expression = ''; s.expressionRequest = null; draw('linked'); return;
    }
    var el = document.getElementById(key) || picker;
    var group = el && el.closest('.cv-ctl');
    if (!group || !group.contains(document.activeElement)) return;
    var controls = linkedControls(rgb);
    s.expressionRequest = { rgb: rgb, controls: controls };
    var genes = Object.keys(controls).filter(function (id) { return id !== 'coordviews_expression_mode'; })
      .map(function (id) { return controls[id]; }).flat().filter(Boolean);
    s.expression = genes.length ? 'Updating expression: ' + genes.join(', ') + '…' : 'No expression selected';
    draw('linked');
  }
  function install() {
    if (installed || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    installed = true;
    window.Shiny.addCustomMessageHandler('cerebro_view_feedback', function (message) {
      if (!specs[message.id] || !fresh(message) || message.renderToken == null) return;
      var s = state(message.id);
      s.frames.set(message.renderToken, message.controls || {});
      if (s.terminal) {
        s.recoverToken = message.renderToken;
        if (s.painted === message.renderToken) s.terminal = '';
      }
      while (s.frames.size > 8) s.frames.delete(s.frames.keys().next().value);
      settle(message.id); draw(message.id);
    });
    window.Shiny.addCustomMessageHandler('cerebro_linked_expression_feedback', function (message) {
      if (!fresh(message)) return;
      var s = state('linked'), request = s.expressionRequest;
      if (!request || !!message.rgb !== request.rgb) return;
      if (!Object.keys(request.controls).every(function (key) {
        return normal(request.controls[key]) === normal(message.controls[key]);
      })) return;
      var genes = Object.keys(request.controls).filter(function (id) { return id !== 'coordviews_expression_mode'; })
        .map(function (id) { return request.controls[id]; }).flat().filter(Boolean);
      s.expression = !genes.length ? 'No expression selected' : message.ok ?
        'Expression loaded: ' + genes.join(', ') : 'Expression unavailable for this selection';
      draw('linked');
    });
  }
  function terminal(id, text, kind) {
    var s = state(id);
    s.terminal = text;
    s.terminalKind = kind || 'error';
    s.recoverToken = null;
    Object.keys(s.pending).forEach(function (key) { mark(key, false); });
    s.pending = {};
    draw(id);
    window.dispatchEvent(new CustomEvent('cerebro:view-terminal', { detail: { page: specs[id][0] } }));
  }
  function init() {
    Object.keys(specs).forEach(draw);
    document.addEventListener('click', function (event) {
      var close = event.target.closest('[data-layout-dismiss]');
      var settings = event.target.closest('[data-layout-settings]');
      if (close) dismissLayout(close.getAttribute('data-layout-dismiss'));
      if (settings) openSquareSetting(settings.getAttribute('data-layout-settings'));
    });
    document.addEventListener('change', function (event) {
      Object.keys(layoutControls).forEach(function (id) {
        if (event.target.id !== layoutControls[id][1]) return;
        state(id).wideLayout = false;
        // Respect an explicit choice to return to fluid width.
        if (!event.target.checked && event.isTrusted) dismissLayout(id);
        draw(id);
      });
    });
    window.addEventListener('cerebro:plot-layout', function (event) {
      var d = event.detail;
      if (!d || !layoutControls[d.id] || !fresh(d)) return;
      clearTimeout(layoutTimers[d.id]);
      if (d.square || !d.wide) { state(d.id).wideLayout = false; draw(d.id); return; }
      layoutTimers[d.id] = setTimeout(function () {
        if (!fresh(d) || !window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
        state(d.id).wideLayout = true; draw(d.id);
      }, 500);
    });
    install();
    if (window.jQuery) window.jQuery(document)
      .on('change.cerebroViewFeedback shiny:inputchanged.cerebroViewFeedback', changed)
      .on('shiny:connected.cerebroViewFeedback', install)
      .on('shown.bs.tab.cerebroViewFeedback', function () { Object.keys(specs).forEach(draw); })
      .on('shiny:value.cerebroViewFeedback', function (event) {
        if (event.target.id !== 'hla_motif_note') return;
        // Shiny inserts the HTML after delivering the value event.
        window.setTimeout(function () {
          var marker = event.target.querySelector('[data-view-state]');
          if (marker) terminal('hla_motif_network', marker.textContent.trim(), marker.getAttribute('data-view-state'));
        }, 0);
      })
      .on('shiny:error.cerebroViewFeedback', function (event) {
        if (!event.error || !event.error.message) return;
        var pane = event.target.closest('.tab-pane');
        var id = Object.keys(specs).find(function (id) { return pane && pane.id === 'shiny-tab-' + specs[id][0]; });
        if (id && /projection|motif_note/.test(event.target.id)) terminal(id, 'Unable to update this view. ' + event.error.message);
      });
    window.addEventListener('cerebro:cell-view-ready', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !d.painted || !fresh(d)) return;
      var s = state(d.id);
      if (s.terminal && s.recoverToken !== d.renderToken) return;
      s.ready = true; s.terminal = ''; s.painted = d.renderToken;
      s.empty = d.pointCount === 0;
      settle(d.id); draw(d.id);
    });
    window.addEventListener('cerebro:specialist-state', function (event) {
      var d = event.detail;
      if (!d || !specs[d.viewId] || !fresh(d)) return;
      state(d.viewId).selection = d.selectionPending ? 'Loading identities for summaries and table…' : '';
      draw(d.viewId);
    });
    window.addEventListener('cerebro:linkedviews-ready', function (event) {
      var d = event.detail;
      if (!d || !fresh(d)) return;
      var s = state('linked');
      if (d.primaryReady && d.painted) s.ready = true;
      s.auxiliary = s.ready && !d.complete ? 'Loading annotations and additional views…' : '';
      draw('linked');
    });
    window.addEventListener('cerebro:linkedviews-selection', function (event) {
      var d = event.detail;
      if (!d || !fresh(d)) return;
      state('linked').selection = '';
      draw('linked');
    });
    window.addEventListener('cerebro:view-operation', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !fresh(d)) return;
      state(d.id).operation = d.text || ''; draw(d.id);
    });
    window.addEventListener('cerebro:view-appearance', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !fresh(d)) return;
      var s = state(d.id), prefix = d.id + '_';
      Object.keys(d.values).forEach(function (key) {
        var input = prefix + (key === 'draw_border' ? 'point_border' : key);
        if (s.pending[input] && normal(d.values[key]) === s.pending[input].value) {
          delete s.pending[input]; mark(input, false);
        }
      });
      draw(d.id);
    });
    window.addEventListener('cerebro:dataset-context', function (event) {
      var d = event.detail || {};
      if (!d.changed && d.phase === 'ready') return;
      Object.keys(states).forEach(function (id) { Object.keys(states[id].pending).forEach(function (key) { mark(key, false); }); });
      Object.keys(layoutTimers).forEach(function (id) { clearTimeout(layoutTimers[id]); });
      layoutTimers = {};
      states = {}; Object.keys(specs).forEach(draw);
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
}());
