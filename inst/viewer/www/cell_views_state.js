/* Pure state transitions shared by specialist and linked cell views. */
(function () {
  var S = {};

  S.spaceByRole = function (spaces, role) {
    var ids = Object.keys(spaces || {});
    for (var i = 0; i < ids.length; i++) {
      var space = spaces[ids[i]];
      if (space && (space.id === role || space._role === role)) return space;
    }
    return null;
  };

  S.lensForSpace = function (saved, spaceId, fallbackIndex) {
    saved = saved || [];
    for (var i = 0; i < saved.length; i++) {
      if (saved[i].spaceId === spaceId) return saved[i];
    }
    var identified = saved.some(function (lens) { return !!lens.spaceId; });
    return identified ? null : saved[fallbackIndex] || null;
  };

  S.clearExpression = function (state, mode) {
    if (mode === 'gene') state.gene = null;
    if (mode === 'panels') state.genePanels = null;
    if (mode === 'rgb') state.rgb = null;
    return state;
  };

  S.genePanelSpaceId = function (geneIndex, baseSpaceId) {
    return '__linked_gene_' + geneIndex + '::' + baseSpaceId;
  };

  S.trekkerGeneControls = function (activeView, gene) {
    if (activeView !== 'trekker_projection' || !gene) return null;
    return { trekker_mode: 'gene', trekker_gene_pick: gene };
  };

  S.shouldStashSingleState = function (activeView, targetView, preserveTarget) {
    return !preserveTarget || activeView !== targetView;
  };

  S.expandSparse = function (indices, values, length, defaultValue) {
    var out = new Int32Array(length);
    if (defaultValue) out.fill(defaultValue);
    var count = Math.min(indices ? indices.length : 0, values ? values.length : 0);
    for (var i = 0; i < count; i++) {
      var index = Number(indices[i]);
      if (index >= 0 && index < length) out[index] = Number(values[i]);
    }
    return out;
  };

  S.sharedBase = function (current, fingerprint, cellCount, orderFingerprint) {
    if (current && current.datasetFingerprint === fingerprint &&
        current.cellCount === cellCount &&
        current.orderFingerprint === orderFingerprint) return current;
    return {
      datasetFingerprint: fingerprint,
      cellCount: cellCount,
      orderFingerprint: orderFingerprint,
      projections: Object.create(null),
      gpuPositions: Object.create(null)
    };
  };

  S.sharedProjection = function (shared, name, count) {
    var projection = shared && shared.projections && shared.projections[name];
    if (!projection || !projection.x || !projection.y ||
        projection.x.length !== count || projection.y.length !== count ||
        (projection.z && projection.z.length !== count)) return null;
    return projection;
  };

  S.canonicalGroupedValues = function (grouped, groups, fallback) {
    if (!ArrayBuffer.isView(groups)) return null;
    if (ArrayBuffer.isView(grouped)) {
      return grouped.length === groups.length ? grouped : null;
    }
    if (!Array.isArray(grouped)) return null;
    var out = new Array(groups.length);
    var positions = new Uint32Array(grouped.length);
    for (var i = 0; i < groups.length; i++) {
      var group = Number(groups[i]);
      var values = grouped[group] || [];
      var value = values[positions[group]++];
      out[i] = value == null ? fallback : value;
    }
    return out;
  };

  S.canonicalAuxValues = function (values, groups, fallback) {
    if (!ArrayBuffer.isView(groups)) return null;
    if (ArrayBuffer.isView(values)) {
      return values.length === groups.length ? values : null;
    }
    if (!Array.isArray(values)) return null;
    var first = values.length ? values[0] : null;
    var nested = Array.isArray(first) || ArrayBuffer.isView(first);
    if (!nested && values.length === groups.length) return values;
    return S.canonicalGroupedValues(values, groups, fallback);
  };

  // Project the live client-side selection into the state that may safely be
  // published to Shiny. Specialist canvases can paint before their deferred
  // stable cell identities arrive; during that window the local selection is
  // real, but downstream selected-cell results are not ready yet.
  S.specialistSelectionReport = function (selection, cells, cellCount) {
    var indices = selection ? Array.from(selection) : [];
    var hasSelection = indices.length > 0;
    var stableKeysReady = Array.isArray(cells) && cells.length === cellCount;
    return {
      hasSelection: hasSelection,
      selectedCells: indices.length,
      stableKeysReady: stableKeysReady,
      pending: hasSelection && !stableKeysReady,
      ids: hasSelection && stableKeysReady
        ? indices.map(function (index) { return cells[index]; })
        : null
    };
  };

  S.specialistLifecycle = {
    begin: function (store, id, renderRequestSent) {
      var previous = store[id] || {};
      var timing = {
        generation: (Number(previous.generation) || 0) + 1,
        renderRequestSent: !!renderRequestSent,
        cached: !renderRequestSent
      };
      store[id] = timing;
      return timing;
    },

    update: function (store, id, values) {
      var timing = store[id] || (store[id] = {});
      Object.assign(timing, values || {});
      return timing;
    },

    activate: function (store, id, now) {
      return this.update(store, id, { activationStartedAtMs: now });
    },

    ready: function (store, id, now) {
      var timing = store[id] || {};
      return Object.assign({}, timing, {
        readyAtMs: now,
        requestToReadyMs: isFinite(timing.requestAtMs)
          ? now - timing.requestAtMs : null
      });
    },

    requestAux: function (view) {
      var token = view && view.data && view.data.wire_token;
      if (token == null || view._auxToken === token ||
          view._auxPending === token) return null;
      view._auxPending = token;
      return token;
    },

    acceptAux: function (view, message) {
      if (!view || !view.data || !message ||
          Number(view.data.wire_token) !== Number(message.wire_token)) {
        return false;
      }
      view._auxPending = null;
      view._auxToken = message.wire_token;
      return true;
    }
  };

  window.CBViewState = S;
})();
