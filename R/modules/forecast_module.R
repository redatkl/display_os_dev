# Page Forecast
source("R/modules/forecast/sidebar_forecast.R")
source("R/modules/forecast/map_forecast.R")
source("R/functions/db_config.R")

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
    
    # Store current raster for click extraction
    current_rast <- reactiveVal(NULL)
    
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
      
      current_rast(rast) 
      
      config <- get_forecast_config(variable)
      
      pal <- colorBin(
        palette  = config$colors,
        bins     = config$breaks,
        na.color = "transparent",
        right    = FALSE,
        pretty   = FALSE
      )
      
      leafletProxy("map-forecast_map") %>%
        clearGroup("raster") %>%
        clearControls() %>%
        addRasterImage(rast, colors = pal, opacity = 0.7, group = "raster") %>%
        addLegend(
          position = "bottomright",
          colors   = rev(config$colors),
          labels   = rev(config$labels),
          title    = config$title,
          opacity  = 0.9
        )
      
    }) %>% bindEvent(forecast_vals$update_trigger, ignoreInit = TRUE)

  
  # After rendering the map, add click observer
  observeEvent(input[["map-forecast_map_click"]], {
    click <- input[["map-forecast_map_click"]]
    req(click, !is.null(rast))
    
    rast <- current_rast()
    req(!is.null(rast))
    
    # Extract value at clicked point
    val <- raster::extract(rast, data.frame(x = click$lng, y = click$lat))[1]
    
    unit <- if (forecast_vals$variable == "temp") "°C" else "mm"
    title <- if (forecast_vals$variable == "temp") "Température" else "Précipitations"
    
    if (is.na(val)) {
      content <- "<div style='font-family:Arial;padding:5px;'>Pas de données</div>"
    } else {
      content <- sprintf(
        "<div style='font-family:Arial;padding:5px;'>
          <b>%s</b><br/>
          <span style='font-size:16px;color:#047857;'><b>%.2f %s</b></span><br/>
          <small>Jour %d</small>
        </div>",
        title, val, unit, forecast_vals$day
      )
    }
    
    leafletProxy("map-forecast_map") %>%
      addPopups(click$lng, click$lat, content,
                options = popupOptions(closeButton = TRUE))
  })
  
 })
}