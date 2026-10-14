# Add raster layer to map
add_raster_layer <- function(map_id, raster_obj, indice, zone = NULL) {
  if (is.null(raster_obj)) {
    leafletProxy(map_id) %>% clearGroup("raster") %>% clearGroup("zone") %>% clearControls()
    return(invisible(NULL))
  }

  # Optional clip to an sf polygon (e.g. ANDZOA action zone).
  # Crop first so the mask only works on the zone's bounding box.
  if (!is.null(zone)) {
    zone_sp <- sf::as_Spatial(zone)
    raster_obj <- raster::crop(raster_obj, raster::extent(zone_sp))
    raster_obj <- raster::mask(raster_obj, zone_sp)
  }

  # Get configuration for this indice (breaks, colors, labels)
  config <- get_color_config(indice)
  
  # Clean raster values (handle NA)
  raster_obj <- calc(raster_obj, fun = function(x) {
    x[x < -9999 | is.infinite(x)] <- NA
    return(x)
  })
  
  # Get values for legend (excluding NA)
  vals <- values(raster_obj)
  vals <- vals[!is.na(vals)]
  
  # Create binned color palette instead of continuous
  # right = FALSE means intervals are [a, b) instead of (a, b]
  pal <- colorBin(
    palette = config$colors,
    bins = config$breaks,
    na.color = "transparent",
    right = FALSE,
    pretty = FALSE
  )
  
  proxy <- leafletProxy(map_id) %>%
    clearGroup("raster") %>%
    clearGroup("zone") %>%
    clearControls() %>%
    addRasterImage(
      raster_obj,
      colors = pal,
      opacity = 0.7,
      group = "raster")

  # Zone outline + zoom
  if (!is.null(zone)) {
    bb <- sf::st_bbox(zone)
    proxy %>%
      addPolygons(
        data = zone,
        fill = FALSE,
        color = "#222222",
        weight = 2,
        group = "zone") %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }

  proxy
}