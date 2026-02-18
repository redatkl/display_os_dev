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
    
    conn <- init_db()
    
    # Get reactive values from sidebar
    forecast_vals <- sidebar_forecast_server("sidebar2")
    
    forecast_map <- forecastMapServer("map", initial_zoom = 5)
    
    
    observe({
      req(forecast_vals$update_trigger > 0)
      
      variable <- forecast_vals$variable
      day      <- forecast_vals$day
      
      cat("Rendering forecast: variable =", variable, ", day =", day, "\n")
      
      rast <- fetch_forecast_raster(variable, day, conn)
      
      if (is.null(rast)) {
        leafletProxy("map-forecast_map") %>% clearGroup("raster") %>% clearControls()
        return()
      }
      
      # Clean NoData values
      rast <- calc(rast, fun = function(x) {
        x[x < -9999 | is.infinite(x)] <- NA
        return(x)
      })
      
      config <- get_forecast_color_config(variable)
      
      vals <- values(rast)
      vals <- vals[!is.na(vals)]
      
      # Reverse palette so legend reads bottom=low, top=high
      palette <- if (variable == "temp") 
        c("#0000FF", "#00FFFF", "#FFFF00", "#FF8000", "#FF0000")
      else 
        c("#FFFFFF", "#CCEBFF", "#66C2FF", "#0080FF", "#0040CC", "#00007F")
      
      pal <- colorNumeric(
        palette  = palette,
        domain   = vals,
        reverse  = FALSE,
        na.color = "transparent"
      )
      
      leafletProxy("map-forecast_map") %>%
        clearGroup("raster") %>%
        clearControls() %>%
        addRasterImage(rast, colors = pal, opacity = 0.7, group = "raster") %>%
        addLegend(
          "bottomright",
          pal    = pal,
          values = vals,
          title  = if (variable == "temp") "Température prévue (°C)" else "Précipitations prévues (mm)",
          opacity = 0.9,
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      
    }) %>% bindEvent(forecast_vals$update_trigger, ignoreInit = TRUE)
  })
}