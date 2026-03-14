// map_selector.js
// SVG drill-down map: National → Régional → Provincial → Communal

(function () {

  // ── Property name map (must match your GeoJSON) ─────────────────────────
  const PROPS = {
    regions:   { name: "nom_fr",        code: "id_region" },
    provinces: { name: "Nom_Provinces",  code: "Code_Province" },
    communes:  { name: "commune",        code: "gid" }
  };

  // ── Niveau labels matching your R selectInput choices ───────────────────
  const NIVEAU_MAP = {
    regions:   "Régional",
    provinces: "Provincial",
    communes:  "Communal"
  };

  // ── State per container (supports multiple instances on page) ────────────
  const instances = {};

  function getInstance(ns) {
    return instances[ns];
  }

  // ── Entry point: called once per container found in DOM ──────────────────
  function initMapSelector(container) {
    const ns      = container.dataset.ns;          // e.g. "reporting-analyse-"
    const svgEl   = document.getElementById(ns + "map_svg");
    const bcEl    = document.getElementById(ns + "map_breadcrumb");

    if (!svgEl) return;

    const state = {
      ns,
      container,
      svgEl,
      bcEl,
      currentFile:    "regions",
      currentNiveau:  "National",   // what clicking will SELECT
      filterProp:     null,         // property to filter child GeoJSON by
      filterVal:      null,         // value to filter on
      selectedName:   null,
      history:        []            // for back-navigation
    };

    instances[ns] = state;

    const svg = d3.select(svgEl);
    const g   = svg.append("g").attr("class", "map-g");
    state.svg = svg;
    state.g   = g;

    loadAndRender(state, "regions", null, null);
  }

  // ── Load GeoJSON, optionally filter, then render ─────────────────────────
  function loadAndRender(state, file, filterProp, filterVal) {
    d3.json("/geojson/" + file + ".geojson").then(function (data) {

      let features = data.features;
      if (filterProp && filterVal) {
        features = features.filter(f => f.properties[filterProp] == filterVal);
      }

      if (features.length === 0) {
        console.warn("No features found for filter:", filterProp, filterVal);
        return;
      }

      state.currentFile  = file;
      state.filterProp   = filterProp;
      state.filterVal    = filterVal;

      renderFeatures(state, features);
      updateBreadcrumb(state);
    }).catch(function (err) {
      console.error("GeoJSON load error:", err);
    });
  }

  // ── Render features into the SVG ─────────────────────────────────────────
  function renderFeatures(state, features) {
    const svgEl = state.svgEl;
    const w = svgEl.clientWidth  || svgEl.parentElement.clientWidth  || 300;
    const h = svgEl.clientHeight || svgEl.parentElement.clientHeight || 400;

    const projection = d3.geoMercator()
      .fitSize([w, h], { type: "FeatureCollection", features });
    const path = d3.geoPath().projection(projection);

    // Fade out old paths
    state.g.selectAll("path")
      .transition().duration(180)
      .style("opacity", 0)
      .remove();

    // Draw new paths after fade
    setTimeout(function () {
      const paths = state.g.selectAll("path")
        .data(features, d => d.properties[PROPS[state.currentFile].name])
        .join(
          enter => enter.append("path")
            .attr("class", "map-feature")
            .attr("d", path)
            .style("opacity", 0)
            .on("mouseover", onHover)
            .on("mouseout",  onMouseOut)
            .on("click",     (event, d) => onFeatureClick(event, d, state)),
          update => update
            .attr("d", path)
            .on("click", (event, d) => onFeatureClick(event, d, state)),
          exit => exit.remove()
        );

      // Re-apply selected highlight
      paths.classed("selected", d => {
        const nameProp = PROPS[state.currentFile].name;
        return d.properties[nameProp] === state.selectedName;
      });

      paths.transition().duration(250).style("opacity", 1);

    }, 190);
  }

  // ── Click handler ─────────────────────────────────────────────────────────
  function onFeatureClick(event, d, state) {
    const file    = state.currentFile;
    const props   = PROPS[file];
    const name    = d.properties[props.name];
    const code    = d.properties[props.code];

    // Update selected highlight
    state.g.selectAll("path").classed("selected", false);
    d3.select(event.currentTarget).classed("selected", true);
    state.selectedName = name;

    // Push history for back navigation
    state.history.push({
      file:      state.currentFile,
      niveau:    state.currentNiveau,
      filterProp: state.filterProp,
      filterVal:  state.filterVal,
      selected:  name
    });

    // Determine next drill-down level
    let nextFile, nextFilterProp, nextFilterVal, nextNiveau;

    if (file === "regions") {
      // Clean code: your regions GeoJSON uses id_region as integer
      // provinces use Code_Region like "01." — format to match
      const regionId    = d.properties["id_region"];
      const regionCode  = String(regionId).padStart(2, "0") + ".";
      nextFile          = "provinces";
      nextFilterProp    = "Code_Region";
      nextFilterVal     = regionCode;
      nextNiveau        = "Régional";
      state.currentNiveau = "Régional";
    } else if (file === "provinces") {
      nextFile          = "communes";
      nextFilterProp    = "Nom_Provinces";   // adjust if your communes GeoJSON uses different prop
      nextFilterVal     = name;
      nextNiveau        = "Provincial";
      state.currentNiveau = "Provincial";
    } else {
      // Communes — leaf level, just select
      state.currentNiveau = "Communal";
      notifyShiny(state, "Communal", name);
      updateBreadcrumb(state);
      return;
    }

    // Notify Shiny of the current selection BEFORE drilling down
    notifyShiny(state, state.currentNiveau, name);

    // Drill down
    state.currentFile = nextFile;
    loadAndRender(state, nextFile, nextFilterProp, nextFilterVal);
  }

  // ── Hover handlers ────────────────────────────────────────────────────────
  function onHover(event) {
    d3.select(event.currentTarget).classed("hovered", true);
  }
  function onMouseOut(event) {
    d3.select(event.currentTarget).classed("hovered", false);
  }

  // ── Notify Shiny ──────────────────────────────────────────────────────────
  function notifyShiny(state, niveau, name) {
    if (!window.Shiny) return;
    Shiny.setInputValue(state.ns + "map_selection", {
      niveau: niveau,
      name:   name,
      nonce:  Math.random()
    }, { priority: "event" });
    console.log("[MapSelector] Shiny notified:", niveau, name);
  }

  // ── Breadcrumb update ─────────────────────────────────────────────────────
  function updateBreadcrumb(state) {
    if (!state.bcEl) return;
    const crumbs = ["National"];
    state.history.forEach(h => crumbs.push(h.selected));
    state.bcEl.innerHTML = crumbs.map((c, i) =>
      i < crumbs.length - 1
        ? `<span class="bc-item bc-link" data-index="${i}">${c}</span><span class="bc-sep">›</span>`
        : `<span class="bc-item bc-current">${c}</span>`
    ).join("");

    // Breadcrumb click → navigate back
    state.bcEl.querySelectorAll(".bc-link").forEach(el => {
      el.addEventListener("click", function () {
        const idx = parseInt(this.dataset.index);
        navigateBack(state, idx);
      });
    });
  }

  // ── Back navigation ───────────────────────────────────────────────────────
  function navigateBack(state, toIndex) {
    // toIndex=0 means back to National
    state.history = state.history.slice(0, toIndex);
    state.selectedName = null;

    if (toIndex === 0) {
      state.currentFile   = "regions";
      state.currentNiveau = "National";
      state.filterProp    = null;
      state.filterVal     = null;
      notifyShiny(state, "National", null);
      loadAndRender(state, "regions", null, null);
    } else {
      const prev = state.history[toIndex - 1];
      state.currentFile   = prev.file === "regions" ? "provinces" : "communes";
      state.currentNiveau = prev.niveau;
      const h = state.history[toIndex - 1];
      loadAndRender(state, state.currentFile, h.filterProp, h.filterVal);
    }
    updateBreadcrumb(state);
  }

  // ── Global reset (called by reset button via onclick attr) ───────────────
  window.resetMapSelector = function (ns) {
    const state = instances[ns];
    if (!state) return;
    state.history       = [];
    state.selectedName  = null;
    state.currentFile   = "regions";
    state.currentNiveau = "National";
    state.filterProp    = null;
    state.filterVal     = null;
    notifyShiny(state, "National", null);
    loadAndRender(state, "regions", null, null);
    updateBreadcrumb(state);
  };

  // ── Highlight from Shiny (dropdown → map) ────────────────────────────────
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("highlight_map_feature", function (msg) {
      const state = instances[msg.ns];
      if (!state) return;
      state.g.selectAll("path").classed("selected", function (d) {
        if (!d) return false;
        const nameProp = PROPS[state.currentFile].name;
        return d.properties[nameProp] === msg.name;
      });
      state.selectedName = msg.name;
    });
  }

  // ── Init all containers on DOM ready ────────────────────────────────────
  document.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll("[data-ns][data-niveau]").forEach(initMapSelector);
  });

  // Also init when Shiny reconnects (page tab switching)
  if (window.Shiny) {
    $(document).on("shiny:connected", function () {
      document.querySelectorAll("[data-ns][data-niveau]").forEach(function (container) {
        const ns = container.dataset.ns;
        if (!instances[ns]) initMapSelector(container);
      });
    });
  }

})();