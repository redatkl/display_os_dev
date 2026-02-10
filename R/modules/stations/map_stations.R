# station map module for the stations tab of the app "R/modules/stations/map.R"

# Station map module

stationMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/map_stations.css")
    ),
    
    div(
      class = "station-map-wrapper",
      leafletOutput(ns("station_map"), height = "100%")
    )
  )
}

# server map module for the stations tab of the app "R/modules/stations/map.R"
stationMapServer <- function(id, initial_zoom = 6) {
  moduleServer(id, function(input, output, session) {
    
    zoom_level <- reactiveVal(initial_zoom)
    
    output$station_map <- renderLeaflet({
      leaflet(maroc, options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2,
        maxZoom = 18,
        attributionControl = FALSE,
        zoomControl = FALSE
      )) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addPolygons(
          color = "black",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.2,
          fillColor = "#856531"
        ) %>%
        setView(lng = -7.5, lat = 33.5, zoom = initial_zoom) %>%
        setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            var zoomControl = L.control.zoom({
              position: 'bottomright'
            });
            map.addControl(zoomControl);
          }
        ")
    })
    
    observe({
      leafletProxy("station_map") %>%
        setView(lng = -7.5, lat = 33.5, zoom = zoom_level())
    }) %>% bindEvent(zoom_level())
    
    return(list(
      map_click = reactive(input$station_map_click),
      map_bounds = reactive(input$station_map_bounds),
      set_zoom = function(zoom) {
        zoom_level(zoom)
      }
    ))
  })
}