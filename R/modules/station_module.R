# stations module
source("R/modules/stations/sidebar_stations.R")
source("R/modules/stations/map_stations.R")
#source("R/functions/stations_db.R")


station_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
    .station-popup {
      font-family: Arial, sans-serif;
      min-width: 200px;
    }
    .station-popup .header {
      background: linear-gradient(135deg, #856531 0%, #7e3619 100%);
      color: white;
      padding: 0;
      margin: 0;
      border-radius: 12px 0 12px 0;
      display: flex;           
      flex-direction: column;
    }
    .station-popup .station-id {
      font-size: 11px;
      opacity: 0.9;
      text-align: left;
    }
    .station-popup .station-name {
      font-size: 14px;
      font-weight: bold;
      margin-top: 2px;
      text-align: center;
      order: -1;
    }
    .station-popup .data-row {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 6px 0;
      border-top: 1px solid #eee;
    }
    .station-popup .icon {
      font-size: 20px;
    }
    .station-popup .value {
      font-size: 16px;
      font-weight: bold;
      color: #2196F3;
    }
  "))
    ),
    
    sidebar_stations_ui(ns("sidebar1")),
    
    div(
      class = "station-map-container",
      stationMapUI(ns("map"))
    )
  )
  
}

station_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #weather_conn <- init_weather_db()
    
    # Get reactive values from sidebar
    station_vals <- sidebar_stations_server("sidebar1")
    
    station_map <- stationMapServer("map", initial_zoom = 5)
    
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
              "<div class='station-popup'>",
              "<div class='header'>",
              "<div class='station-id'>ID: ", data$station_id, "</div>",
              "<div class='station-name'>", data$station_name, "</div>",
              "</div>",
              "<div class='data-row'>",
              "<span class='icon'>💧</span>",
              "<span class='value'>", round(data$value, 2), " mm</span>",
              "</div>",
              "</div>"
            ),
            paste0(
              "<div class='station-popup'>",
              "<div class='header'>",
              "<div class='station-id'>ID: ", data$station_id, "</div>",
              "<div class='station-name'>", data$station_name, "</div>",
              "</div>",
              "<div style='padding: 8px; color: #999;'>Pas de données</div>",
              "</div>"
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