# Page Forecast
source("R/modules/forecast/sidebar_forecast.R")

forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "css/forecast.css")
    # ),
    
    sidebar_forecast_ui(ns("sidebar2")),
    
    div(
      class = "forecast-map-container",
      stationMapUI(ns("map"))
    )
    
  )
}

forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Get reactive values from sidebar
    forecast_vals <- sidebar_forecast_server("sidebar2")
    
    
    # Use the parameters here
    cat("Rendering forecsat with params:\n")
    cat("  Source:", forecast_vals$variable, "\n")
    cat("  Date:", as.character(forecast_vals$date), "\n")
    
  })
}