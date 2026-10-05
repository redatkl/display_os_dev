// climat_3d.js
// Morocco in grey, ANDZOA provinces as a clickable 3D layer.
// Click = select the province (sent to Shiny), click again = unselect.

(function () {

  var BASE = "#7fb69a", SELECTED = "#791617", DIM = "#c9d8cf";

  function render(svgId) {
    var svgEl = document.getElementById(svgId);
    if (!svgEl) return;

    var inputId = svgEl.getAttribute("data-input");

    Promise.all([
      d3.json(svgEl.getAttribute("data-morocco-url")),
      d3.json(svgEl.getAttribute("data-zones-url"))
    ]).then(function (res) {
      var morocco = res[0], zones = res[1];

      var svg = d3.select(svgEl);
      svg.selectAll("*").remove();

      var W = 600, H = 700;
      var projection = d3.geoMercator().fitExtent([[20, 20], [W - 20, H - 20]], morocco);
      var path = d3.geoPath().projection(projection);

      var defs = svg.append("defs");
      defs.append("filter").attr("id", svgId + "-shadow")
        .attr("x", "-20%").attr("y", "-20%").attr("width", "140%").attr("height", "150%")
        .append("feDropShadow")
        .attr("dx", 0).attr("dy", 10).attr("stdDeviation", 8)
        .attr("flood-color", "#000").attr("flood-opacity", 0.35);

      svg.append("g")
        .selectAll("path").data(morocco.features).enter().append("path")
        .attr("d", path)
        .attr("fill", "#d1d5db")
        .attr("stroke", "#9ca3af")
        .attr("stroke-width", 0.8);

      var layer = svg.append("g").attr("filter", "url(#" + svgId + "-shadow)");

      var tip = d3.select(svgEl.parentNode).selectAll(".climat-tip").data([0]).join("div")
        .attr("class", "climat-tip");

      var selected = null;

      // Colors + a small lift on the selected province for the 3D effect
      function refresh() {
        paths
          .attr("fill", function (d) {
            if (selected === null) return BASE;
            return d.properties.Code_Province === selected ? SELECTED : DIM;
          })
          .attr("transform", function (d) {
            return d.properties.Code_Province === selected ? "translate(0,-6)" : null;
          });
      }

      var paths = layer.append("g")
        .selectAll("path").data(zones.features).enter().append("path")
        .attr("class", "climat-zone")
        .attr("d", path)
        .attr("stroke", "#fff")
        .attr("stroke-width", 1)
        .on("mousemove", function (event, d) {
          var box = svgEl.parentNode.getBoundingClientRect();
          tip.style("display", "block")
            .style("left", (event.clientX - box.left + 12) + "px")
            .style("top", (event.clientY - box.top + 12) + "px")
            .html("<b>" + d.properties.Nom_Provinces + "</b>");
        })
        .on("mouseleave", function () { tip.style("display", "none"); })
        .on("click", function (event, d) {
          var code = d.properties.Code_Province;
          selected = (selected === code) ? null : code;
          refresh();
          // priority:event so clicking the same province twice still triggers Shiny
          Shiny.setInputValue(inputId, selected === null ? "" : selected, { priority: "event" });
        });

      refresh();
    }).catch(function (err) {
      console.error("Climat 3D map error:", err);
    });
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("climat3d", function (msg) {
      render(msg.id);
    });
  });

})();