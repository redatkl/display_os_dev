# stations module
source("R/modules/stations/sidebar_stations.R")


station_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebar_stations_ui(ns("sidebar1"))
    
    # Main content area for stations can be added here
  )
  
}

station_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Server logic for Expert module goes here
  })
}