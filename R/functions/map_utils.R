# Add raster layer to map
add_raster_layer <- function(map_id, raster_obj, indice) {
  if (is.null(raster_obj)) {
    leafletProxy(map_id) %>% clearGroup("raster") %>% clearControls()
    return(invisible(NULL))
  }
  
  # raster object for leaflet
  vals <- values(raster_obj)
  pal <- colorNumeric(get_palette(indice), vals, na.color = "transparent")
  
  leafletProxy(map_id) %>%
    clearGroup("raster") %>%
    clearControls() %>%
    addRasterImage(
      raster_obj,  
      colors = pal,
      opacity = 0.7,
      group = "raster")
    # ) %>%
    # addLegend(
    #   "bottomleft",
    #   pal = pal,
    #   values = vals,
    #   title = indice,
    #   opacity = 0.7
    # )
}