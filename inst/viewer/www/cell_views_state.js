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

  S.canonicalMapping = {
    datasetIdentityMatches: function (resource, identity) {
      identity = identity || {};
      return resource &&
        /^md5-cell-set-v1:[0-9a-f]{32}$/.test(
          String(resource.dataset_fingerprint || '')
        ) &&
        /^md5-cell-order-v1:[0-9a-f]{32}$/.test(
          String(resource.cell_order_fingerprint || '')
        ) &&
        /^md5-crb-v1:[0-9a-f]{32}$/.test(
          String(resource.pack_dataset_fingerprint || '')
        ) &&
        String(resource.dataset_fingerprint || '') ===
          String(identity.cell_fingerprint || '') &&
        String(resource.cell_order_fingerprint || '') ===
          String(identity.cell_order_fingerprint || '') &&
        String(resource.pack_dataset_fingerprint || '') ===
          String(identity.pack_dataset_fingerprint || '');
    },

    resourceIdentityMatches: function (resource, identity) {
      return this.datasetIdentityMatches(resource, identity) &&
        Number(resource.cells) === Number((identity || {}).cell_count);
    },

    projectionIdentityMatches: function (resource, identity) {
      return resource && resource.protocol === 'canonical-projection-v1' &&
        resource.dtype === 'float32' &&
        this.resourceIdentityMatches(resource, identity);
    },

    subsetContract: function (resource, expectedCanonicalCells, expectedCells) {
      var kind = String(resource && resource.kind || '');
      var canonicalCells = Number(resource && resource.canonical_cells) || 0;
      var cells = Number(resource && resource.cells) || 0;
      if (!resource || resource.protocol !== 'canonical-subset-v1' ||
          resource.dtype !== 'uint32' || Number(resource.index_base) !== 0 ||
          canonicalCells !== Number(expectedCanonicalCells) ||
          cells !== Number(expectedCells) || canonicalCells < cells || cells < 0) {
        return null;
      }
      if (kind === 'identity') {
        return cells === canonicalCells && Number(resource.bytes) === 0
          ? {kind: kind, canonicalCells: canonicalCells, cells: cells,
            expectedCount: 0}
          : null;
      }
      var expectedCount = kind === 'include_uint32' ? cells :
        (kind === 'exclude_uint32' ? canonicalCells - cells : -1);
      if (expectedCount < 0 || !resource.url ||
          Number(resource.bytes) !== expectedCount * 4) return null;
      return {
        kind: kind, canonicalCells: canonicalCells, cells: cells,
        expectedCount: expectedCount
      };
    },

    indicesMatch: function (mapping, indices) {
      if (!mapping || mapping.kind === 'identity') return !!mapping;
      if (!indices || indices.length !== mapping.expectedCount) return false;
      if (mapping.kind === 'exclude_uint32') {
        var previous = -1;
        for (var i = 0; i < indices.length; i++) {
          if (indices[i] >= mapping.canonicalCells || indices[i] <= previous) {
            return false;
          }
          previous = indices[i];
        }
        return true;
      }
      if (mapping.kind !== 'include_uint32') return false;
      var seen = new Uint8Array(mapping.canonicalCells);
      for (var j = 0; j < indices.length; j++) {
        if (indices[j] >= mapping.canonicalCells || seen[indices[j]]) return false;
        seen[indices[j]] = 1;
      }
      return true;
    },

    forEachIndex: function (mapping, indices, visit) {
      var count = 0;
      if (mapping.kind === 'identity') {
        for (var identity = 0; identity < mapping.canonicalCells; identity++) {
          visit(identity); count++;
        }
      } else if (mapping.kind === 'include_uint32') {
        for (var i = 0; i < indices.length; i++) {
          visit(indices[i]); count++;
        }
      } else {
        var excluded = 0;
        for (var canonical = 0; canonical < mapping.canonicalCells; canonical++) {
          if (excluded < indices.length && indices[excluded] === canonical) {
            excluded++;
          } else {
            visit(canonical); count++;
          }
        }
      }
      return count;
    }
  };

  S.resourceDescriptor = {
    validate: function (resource, expected) {
      expected = expected || {};
      if (!resource) return null;
      var cells = Number(resource.cells);
      var dimensions = Number(resource.dimensions);
      var dtype = String(resource.dtype || '');
      if (expected.protocol != null &&
          resource.protocol !== expected.protocol) return null;
      if (expected.cells != null && cells !== Number(expected.cells)) return null;
      if (expected.minCells != null && cells < Number(expected.minCells)) return null;
      if (expected.dtype != null && dtype !== expected.dtype) return null;
      if (expected.dtypes && expected.dtypes.indexOf(dtype) < 0) return null;
      if (expected.dimensions != null &&
          dimensions !== Number(expected.dimensions)) return null;
      if (expected.dimensionSet &&
          expected.dimensionSet.indexOf(dimensions) < 0) return null;
      if (expected.requireUrl && !resource.url) return null;
      if (expected.identity &&
          !S.canonicalMapping.datasetIdentityMatches(
            resource, expected.identity
          )) return null;
      return {
        resource: resource,
        url: resource.url == null ? null : String(resource.url),
        checksum: String(resource.checksum || ''),
        bytes: Number(resource.bytes) || 0,
        cells: cells,
        dimensions: dimensions,
        dtype: dtype
      };
    },

    codeWidth: function (descriptor) {
      if (!descriptor) return 0;
      return descriptor.dtype === 'uint8' ? 1 :
        (descriptor.dtype === 'uint16' ? 2 :
          (descriptor.dtype === 'uint32' ? 4 : 0));
    }
  };

  S.telemetry = {
    number: function (value) {
      value = Number(value);
      return isFinite(value) ? value : null;
    },

    transport: function (profile, values, includeServerBreakdown, nowEpoch) {
      profile = profile || {};
      values = values || {};
      var sentAt = Number(profile.sent_at_ms);
      var metric = {
        bytes: Number(values.bytes) || 0,
        serverPrepareMs: this.number(profile.server_prepare_ms),
        serializeTransferMs: isFinite(sentAt)
          ? Number(nowEpoch) - sentAt : null,
        decodeMs: this.number(values.decodeMs)
      };
      if (includeServerBreakdown) {
        metric.serverResourceMs = this.number(profile.server_resource_ms);
        metric.serverBundleMs = this.number(profile.server_bundle_ms);
      }
      return Object.assign(metric, values);
    },

    snapshot: function (metric, values) {
      return Object.assign({}, metric || {}, values || {});
    }
  };

  window.CBViewState = S;
})();
