# Add raster layer to map
add_raster_layer <- function(map_id, raster_obj, indice) {
  if (is.null(raster_obj)) {
    leafletProxy(map_id) %>% clearGroup("raster") %>% clearControls()
    return(invisible(NULL))
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
  
  leafletProxy(map_id) %>%
    clearGroup("raster") %>%
    clearControls() %>%
    addRasterImage(
      raster_obj,  
      colors = pal,
      opacity = 0.7,
      group = "raster")%>%
    addLegend(
      "bottomleft",
      colors = config$colors,
      labels = config$labels,
      title = get_indice_title(indice),
      opacity = 0.9
    )
}