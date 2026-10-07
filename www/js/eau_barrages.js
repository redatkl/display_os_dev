// eau_barrages.js
// Morocco in grey, ANDZOA zone in green, dams as circles (size = capacity, colour = fill rate).
// Position, radius, colour and tooltip of each dam are computed in R (eau_ressources.R).

(function () {

  function render(msg) {
    var svgEl = document.getElementById(msg.id);
    if (!svgEl) return;

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

      svg.append("g")
        .selectAll("path").data(morocco.features).enter().append("path")
        .attr("d", path)
        .attr("fill", "#d1d5db")
        .attr("stroke", "#9ca3af")
        .attr("stroke-width", 0.8);

      svg.append("g")
        .selectAll("path").data(zones.features).enter().append("path")
        .attr("d", path)
        .attr("fill", "#cfe6d8")
        .attr("stroke", "#fff")
        .attr("stroke-width", 1);

      var tip = d3.select(svgEl.parentNode).selectAll(".eau-tip").data([0]).join("div")
        .attr("class", "eau-tip");

      svg.append("g")
        .selectAll("circle").data(msg.dams).enter().append("circle")
        .attr("class", "eau-dam")
        .attr("cx", function (d) { return projection([d.lon, d.lat])[0]; })
        .attr("cy", function (d) { return projection([d.lon, d.lat])[1]; })
        .attr("r", function (d) { return d.r; })
        .attr("fill", function (d) { return d.color; })
        .attr("fill-opacity", 0.9)
        .attr("stroke", "#374151")
        .attr("stroke-width", 0.8)
        .on("mousemove", function (event, d) {
          var box = svgEl.parentNode.getBoundingClientRect();
          tip.style("display", "block")
            .style("left", (event.clientX - box.left + 12) + "px")
            .style("top", (event.clientY - box.top + 12) + "px")
            .html(d.tip);
        })
        .on("mouseleave", function () { tip.style("display", "none"); });
    }).catch(function (err) {
      console.error("Eau barrages map error:", err);
    });
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("eauDams", render);
  });

})();