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

  window.CBViewState = S;
})();
