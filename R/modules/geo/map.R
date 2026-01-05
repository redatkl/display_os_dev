# the map module 


# Map module ui
mapModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include custom CSS and JS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/map.css")
    ),

    div(
      class = "map-wrapper",
      leafletOutput(ns("map"), height = "100%")
    )
  )
}

mapModuleServer <- function(id, data, initial_zoom = 5) {
  moduleServer(id, function(input, output, session) {
    
    
    # Reactive value to store zoom level
    zoom_level <- reactiveVal(initial_zoom)

    # Create the leaflet map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2,
        maxZoom = 18,
        attributionControl = FALSE,
        zoomControl = FALSE
      )) %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        #addProviderTiles(providers$Esri.WorldImagery.Labels) %>%
        setView(lng = -17, lat = 28, zoom = initial_zoom)%>%  # Morocco center
        setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)%>%
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            
            // Create zoom control at bottom right
            var zoomControl = L.control.zoom({
              position: 'bottomright'
            });
            
            // Add it to the map
            map.addControl(zoomControl);
          }
        ")
      
    })
    
    # Update zoom when layout changes
    observe({
      leafletProxy("map") %>%
        setView(lng = -17, lat = 28, zoom = zoom_level())
    }) %>% bindEvent(zoom_level())
    
    # Return reactive values for interaction
    return(list(
      map_click = reactive(input$map_click),
      map_bounds = reactive(input$map_bounds),
      set_zoom = function(zoom) {
        zoom_level(zoom)
      }
    ))
  })
}
