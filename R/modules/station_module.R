# stations module
source("R/modules/stations/sidebar_stations.R")
source("R/modules/stations/map_stations.R")
source("R/functions/stations_db.R")


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
    
    weather_conn <- init_weather_db()
    
    # Get reactive values from sidebar
    station_vals <- sidebar_stations_server("sidebar1")
    
    station_map <- stationMapServer("map", initial_zoom = 6)
    
    # Observe when update is triggered
    observe({
      req(station_vals$update_trigger > 0)
      
      if (station_vals$data_source == "station_phys") {
        # Fetch all stations with data
        data <- fetch_stations_data_with_values(
          weather_conn, 
          station_vals$date,
          station_vals$variable
        )
        
        if (nrow(data) > 0) {
          # Variable labels
          var_labels <- list(
            temp = "Temp (°C)",
            humidity = "Hum (%)",
            precip = "Precip (mm)",
            wind = "Vent (m/s)",
            solar = "Solar (W/m²)"
          )
          
          label <- var_labels[[station_vals$variable]]
          
          # Color stations: blue if has data, gray if no data
          colors <- ifelse(!is.na(data$value), "green", "red")
          
          # Create popups
          popups <- ifelse(
            !is.na(data$value),
            paste0(
              "<b>", data$station_name, "</b><br>",
              "Province: ", ifelse(is.na(data$province), "N/A", data$province), "<br>",
              label, ": ", round(data$value, 2), "<br>",
              "Mesures: ", data$n_records
            ),
            paste0(
              "<b>", data$station_name, "</b><br>",
              "Province: ", ifelse(is.na(data$province), "N/A", data$province), "<br>",
              "Pas de données pour cette date"
            )
          )
          
          leafletProxy("map-station_map") %>%
            clearMarkers() %>%
            addCircleMarkers(
              lng = data$longitude,
              lat = data$latitude,
              radius = 6,
              fillOpacity = 0.7,
              color = colors,
              fillColor = colors,
              weight = 2,
              popup = popups
            )
        }
      }
      
      # Use the parameters here
      cat("Rendering with params:\n")
      cat("  Source:", station_vals$data_source, "\n")
      cat("  Variable:", station_vals$variable, "\n")
      cat("  Date:", as.character(station_vals$date), "\n")
      
      
      
    }) %>% bindEvent(station_vals$update_trigger, ignoreInit = TRUE)
  })
}