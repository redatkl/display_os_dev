# geomonitoring module
source("R/modules/geo/sidebar.R")

geo_ui <- function(id) {
  ns <- NS(id)

    #tags$h2("Geomonitoring Page - Under Construction", class = "page-title")

  sidebarModuleUI("sidebar1")
  
}

geo_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sidebar_values <- sidebarModuleServer("sidebar1")
    
  })
}