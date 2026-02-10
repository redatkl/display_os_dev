# Get all stations with coordinates from stations_meteo
get_all_stations <- function(conn) {
  query <- "
    SELECT 
      id as station_id,
      name as station_name,
      province,
      ST_X(geom) as longitude,
      ST_Y(geom) as latitude
    FROM stations_meteo
    ORDER BY name"
  
  dbGetQuery(conn, query)
}

# Fetch data for all stations on a specific date with selected variable
fetch_stations_data_with_values <- function(conn, date, variable) {
  var_col <- get_variable_column(variable)
  
  query <- sprintf("
    SELECT 
      s.id as station_id,
      s.name as station_name,
      s.province,
      ST_X(s.geom) as longitude,
      ST_Y(s.geom) as latitude,
      AVG(w.%s) as value,
      COUNT(w.id) as n_records
    FROM stations_meteo s
    LEFT JOIN p_weather_data w ON s.id = w.station_id 
      AND DATE(w.datetime) = '%s'
    GROUP BY s.id, s.name, s.province, s.geom
    ORDER BY s.name",
                   var_col, date
  )
  
  dbGetQuery(conn, query)
}

# Get variable column name
get_variable_column <- function(variable) {
  vars <- list(
    temp = "hc_air_temperature_avg_degc",
    humidity = "hc_relative_humidity_avg",
    precip = "precipitation_mm",
    wind = "wind_speed_avg_m_per_s",
    solar = "solar_radiation_w_per_m2"
  )
  vars[[variable]]
}