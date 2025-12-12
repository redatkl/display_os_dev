# geomonitoring module
source("R/modules/geo/sidebar.R")
source("R/modules/geo/map_layout_module.R")

geo_ui <- function(id) {
  ns <- NS(id)

    
tagList(
  sidebarModuleUI(ns("sidebar1")),
  
  # Map layout area
  mapLayoutModuleUI(ns("map_layout"))
  
  )
}

geo_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    sidebar_values <- sidebarModuleServer("sidebar1")
    
    # Create a reactive for map layout that defaults to "layout1"
    selected_layout <- reactive({
      layout <- input$`sidebar1-map_layout_selected`
      cat("Selected layout from sidebar:", layout, "\n")  # Debug
      if (is.null(layout) || layout == "") {
        "layout1"  # Default
      } else {
        layout
      }
    })
    
    # Initialize map layout with reactive layout from sidebar
    map_modules <- mapLayoutModuleServer(
      "map_layout",
      layout = selected_layout
    )
    
    # Optional: Access map interactions
    observeEvent(map_modules$map1$map_click(), {
      click <- map_modules$map1$map_click()
      if (!is.null(click)) {
        cat("Map 1 clicked at:", click$lat, click$lng, "\n")
      }
    })
  })
}