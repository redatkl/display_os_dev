# Page Forecast
source("R/modules/forecast/sidebar_forecast.R")
source("R/modules/forecast/map_forecast.R")

forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "css/forecast.css")
    # ),
    
    sidebar_forecast_ui(ns("sidebar2")),
    
    div(
      class = "forecast-map-container",
      forecastMapUI(ns("map"))
    )
    
  )
}

forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Get reactive values from sidebar
    forecast_vals <- sidebar_forecast_server("sidebar2")
    
    forecast_map <- forecastMapServer("map", initial_zoom = 5)
    
    
    observe({
      req(forecast_vals$update_trigger > 0)
    # Use the parameters here
    cat("Rendering forecsat with params:\n")
    cat("  Source:", forecast_vals$variable, "\n")
    cat("  Date:", as.character(forecast_vals$date), "\n")
    
    }) %>% bindEvent(forecast_vals$update_trigger)
    
  })
}