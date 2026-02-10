# stations module
source("R/modules/stations/sidebar_stations.R")
source("R/modules/stations/map_stations.R")


station_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebar_stations_ui(ns("sidebar1")),
    
    div(
      class = "station-map-container",
      stationMapUI(ns("map"))
    )
  )
  
}

station_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Get reactive values from sidebar
    station_vals <- sidebar_stations_server("sidebar1")
    
    station_map <- stationMapServer("map", initial_zoom = 6)
    
    # Observe when update is triggered
    observe({
      req(station_vals$update_trigger > 0)
      
      # Use the parameters here
      cat("Rendering with params:\n")
      cat("  Source:", station_vals$data_source, "\n")
      cat("  Variable:", station_vals$variable, "\n")
      cat("  Date:", as.character(station_vals$date), "\n")
      
      
      
    }) %>% bindEvent(station_vals$update_trigger, ignoreInit = TRUE)
  })
}