// territoire_3d.js
// Draws Morocco in grey, then the action zones on top with a drop shadow.

(function () {

  // Same rule as the R side: dattes+argan = common, dattes only = oasis, else = argan
  function zoneOf(p) {
    if (p.zone_dattes === "TRUE" && p.zone_commune_datt_argan_txt === "TRUE") return 2;
    if (p.zone_dattes === "TRUE") return 0;
    return 1;
  }

  function render(svgId) {
    var svgEl = document.getElementById(svgId);
    if (!svgEl) return;

    var levels = JSON.parse(svgEl.getAttribute("data-levels"));
    var colors = JSON.parse(svgEl.getAttribute("data-colors"));

    Promise.all([
      d3.json(svgEl.getAttribute("data-morocco-url")),
      d3.json(svgEl.getAttribute("data-zones-url"))
    ]).then(function (res) {
      var morocco = res[0], zones = res[1];

      var svg = d3.select(svgEl);
      svg.selectAll("*").remove();

      var W = 600, H = 700;
      var projection = d3.geoMercator().fitExtent(
        [[20, 20], [W - 20, H - 20]], morocco
      );
      var path = d3.geoPath().projection(projection);

      // Soft drop shadow under the whole zones layer
      var defs = svg.append("defs");
      defs.append("filter").attr("id", svgId + "-shadow")
        .attr("x", "-20%").attr("y", "-20%").attr("width", "140%").attr("height", "150%")
        .append("feDropShadow")
        .attr("dx", 0).attr("dy", 10).attr("stdDeviation", 8)
        .attr("flood-color", "#000").attr("flood-opacity", 0.35);

      // 1. Morocco in grey (flat base)
      svg.append("g").attr("class", "morocco-base")
        .selectAll("path").data(morocco.features).enter().append("path")
        .attr("d", path)
        .attr("fill", "#d1d5db")
        .attr("stroke", "#9ca3af")
        .attr("stroke-width", 0.8);

      // 2. Zones with a drop shadow
      var layer = svg.append("g").attr("filter", "url(#" + svgId + "-shadow)");

      var tip = d3.select(svgEl.parentNode).selectAll(".territoire-tip").data([0]).join("div")
        .attr("class", "territoire-tip");

      layer.append("g")
        .selectAll("path").data(zones.features).enter().append("path")
        .attr("class", "zone-top")
        .attr("d", path)
        .attr("fill", function (d) { return colors[zoneOf(d.properties)]; })
        .attr("stroke", "#fff")
        .attr("stroke-width", 1)
        .on("mousemove", function (event, d) {
          var box = svgEl.parentNode.getBoundingClientRect();
          tip.style("display", "block")
            .style("left", (event.clientX - box.left + 12) + "px")
            .style("top", (event.clientY - box.top + 12) + "px")
            .html("<b>" + d.properties.Nom_Provinces + "</b><br>" + levels[zoneOf(d.properties)]);
        })
        .on("mouseleave", function () { tip.style("display", "none"); });
    }).catch(function (err) {
      console.error("Territoire 3D map error:", err);
    });
  }

  // Shiny sends the namespaced svg id once the page is flushed
  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("territoire3d", function (msg) {
      render(msg.id);
    });
  });

})();