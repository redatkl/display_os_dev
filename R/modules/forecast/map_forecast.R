# forecast map module for the forecast tab of the app "R/modules/forecast/map_forecast.R"

# forecast map module

forecastMapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/map_forecast.css")
    ),
    
    div(
      class = "forecast-map-wrapper",
      leafletOutput(ns("forecast_map"), height = "100%")
    )
  )
}

# server map module for the forecast tab of the app "R/modules/forecast/map.R"
forecastMapServer <- function(id, initial_zoom = 5) {
  moduleServer(id, function(input, output, session) {
    
    zoom_level <- reactiveVal(initial_zoom)
    
    output$forecast_map <- renderLeaflet({
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
          fillColor = "transparent"
        ) %>%
        setView(lng = -17, lat = 28, zoom = initial_zoom) %>%
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
      leafletProxy("forecast_map") %>%
        setView(lng = -17, lat = 28, zoom = zoom_level())
    }) %>% bindEvent(zoom_level())
    
    return(list(
      map_click = reactive(input$forecast_map_click),
      map_bounds = reactive(input$forecast_map_bounds),
      set_zoom = function(zoom) {
        zoom_level(zoom)
      }
    ))
  })
}