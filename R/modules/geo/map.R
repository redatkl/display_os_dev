# the map module 


# Map module ui
mapModuleUI <- function(id) {
  ns <- NS(id)
  
    div(
      class = "map-wrapper",
      leafletOutput(ns("map"), height = "100%")
    )
  
}

mapModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    

    # Create the leaflet map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -6.5, lat = 34, zoom = 6)  # Morocco center
    })
    
    # Return reactive values for interaction
    return(list(
      map_click = reactive(input$map_click),
      map_bounds = reactive(input$map_bounds)
    ))
  })
}
