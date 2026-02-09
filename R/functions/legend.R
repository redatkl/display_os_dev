# R/functions/legend_component.R

#' Create a dynamic legend HTML element
#' @param indice The selected indice code (e.g., "precip", "SPI", "NDVI")
#' @param config The color configuration from get_color_config()
#' @return HTML tagList with the legend
create_dynamic_legend <- function(indice, config) {
  if (is.null(config)) {
    return(div(
      class = "legend-empty",
      "Aucune légende disponible"
    ))
  }
  
  # Create legend items
  legend_items <- mapply(function(color, label, i) {
    div(
      class = "legend-item",
      style = sprintf("--index: %d", i),
      div(class = "legend-color-box", style = sprintf("background-color: %s", color)),
      div(class = "legend-label", label)
    )
  }, config$colors, config$labels, seq_along(config$colors), SIMPLIFY = FALSE)
  
  # Get title for the indice
  title <- get_indice_title(indice)
  
  tagList(
    div(
      class = "dynamic-legend-container",
      `data-indice` = indice,
      div(class = "legend-title", title),
      div(class = "legend-items-wrapper", do.call(tagList, legend_items))
    )
  )
}

#' Create placeholder legend (initial state)
create_placeholder_legend <- function() {
  div(
    class = "dynamic-legend-container legend-placeholder",
    div(class = "legend-title", "Sélectionnez un indice"),
    div(class = "legend-message", "La légende s'affichera ici")
  )
}