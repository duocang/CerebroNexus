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
