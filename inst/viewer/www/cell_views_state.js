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

  S.createSinglePreparedCache = function (limit) {
    var entry = null;
    var enabled = limit == null || Number(limit) > 0;
    var valid = function (id, token) {
      return enabled && id != null && token != null && String(token) !== '';
    };
    var matches = function (id, token) {
      return entry && entry.id === String(id) && entry.token === String(token);
    };
    return {
      resolve: function (id, token, build) {
        if (!valid(id, token)) return build();
        if (!matches(id, token)) {
          entry = { id: String(id), token: String(token), value: build() };
        }
        return entry.value;
      },
      update: function (id, token, patch) {
        if (!matches(id, token)) return false;
        var updated = patch(entry.value);
        if (updated === false) entry = null;
        return updated;
      },
      clear: function () { entry = null; },
      size: function () { return entry ? 1 : 0; }
    };
  };

  S.patchSinglePreparedAux = function (prepared, cells, hover, nestedLengths) {
    if (!prepared || !prepared.data || !Array.isArray(cells) ||
        cells.length !== prepared.data.n) return false;
    prepared.data.cells = cells;
    var offsets = null;
    if (Array.isArray(nestedLengths)) {
      offsets = new Uint32Array(nestedLengths.length + 1);
      nestedLengths.forEach(function (length, index) {
        offsets[index + 1] = offsets[index] + Number(length || 0);
      });
    }
    hover = hover || {};
    var modes = hover.hoverinfo;
    var enabled = Array.isArray(modes)
      ? modes.some(function (mode) { return mode !== 'skip'; })
      : modes !== 'skip';
    (prepared.spaceIds || []).forEach(function (spaceId) {
      var space = prepared.spaceById && prepared.spaceById[spaceId];
      if (!space) return;
      space._hover = Array.isArray(hover.text) ? hover.text : [];
      space._hoverColumns = Array.isArray(hover.columns) ? hover.columns : [];
      space._hoverModes = modes;
      space._hoverOffsets = offsets;
      space._hoverEnabled = enabled;
      space._hoverMask = null;
    });
    return true;
  };

  window.CBViewState = S;
})();
