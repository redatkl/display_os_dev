# Table mapping
get_table <- function(indice, temp) {
  suffix <- switch(temp, 
                   "mensuel" = "1month", 
                   "trimestriel" = "3month", 
                   "annuel" = "12month", 
                   ""
  )
  
  tables <- list(
    precip = "morocco_chirps",
    SPI = paste0("morocco_spi", suffix),
    LST = "morocco_lst",
    LST_A = paste0("morocco_alst_", suffix),
    NDVI = "morocco_ndvi",
    ANDVI = paste0("morocco_andvi_", suffix),
    SM = "morocco_surface_sm",
    SM_ROOT = "morocco_root_zone_sm",
    SM_A_SURFACE = paste0("morocco_asurface_sm_", suffix),
    SM_A_ROOT = paste0("morocco_aroot_zone_sm_", suffix),
    CDI = paste0("morocco_cdi_", suffix)
  )
  
  tables[[indice]]
}

# Parse date (same as before)
parse_date <- function(date_str, temp) {
  if (temp == "annuel") {
    list(year = as.integer(date_str), month = NULL, quarter = NULL)
  } else if (temp == "trimestriel") {
    parts <- strsplit(date_str, "-T")[[1]]
    list(year = as.integer(parts[1]), month = NULL, quarter = as.integer(parts[2]))
  } else {
    months <- c("janvier", "février", "mars", "avril", "mai", "juin",
                "juillet", "août", "septembre", "octobre", "novembre", "décembre")
    parts <- strsplit(date_str, " ")[[1]]
    list(year = as.integer(parts[2]), month = match(tolower(parts[1]), months), quarter = NULL)
  }
}

# Fetch raster - USING pgGetRast
fetch_raster <- function(indice, temp, date_str, conn) {
  table <- get_table(indice, temp)
  if (is.null(table)) {
    warning(paste("Table not found for", indice, temp))
    return(NULL)
  }
  
  date <- parse_date(date_str, temp)
  
  # Build WHERE clause
  where_clause <- if (!is.null(date$quarter)) {
    sprintf("WHERE year = %d AND quarter = %d", date$year, date$quarter)
  } else if (!is.null(date$month)) {
    sprintf("WHERE year = %d AND month = %d", date$year, date$month)
  } else {
    sprintf("WHERE year = %d", date$year)
  }
  
  # Fetch raster using pgGetRast
  tryCatch({
    raster_data <- pgGetRast(
      conn,
      name = c("public", table),
      returnclass = "raster",
      clauses = where_clause
    )
    
    if (is.null(raster_data) || length(raster_data) == 0) {
      warning(paste("No data found for", indice, "at", date_str))
      return(NULL)
    }
    
    return(raster_data)
    
  }, error = function(e) {
    warning(paste("Error fetching raster:", e$message))
    return(NULL)
  })
}

# Color palettes (same as before)
# get_palette <- function(indice) {
#   pals <- list(
#     precip = c("#FFFFFF", "#CCEBFF", "#66C2FF", "#0080FF", "#0040CC"),
#     SPI = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90", "#00BFFF"),
#     LST = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000"),
#     LST_A = c("#0000FF", "#FFFFFF", "#FF0000"),
#     NDVI = c("#8B4513", "#D2B48C", "#90EE90", "#228B22", "#006400"),
#     ANDVI = c("#8B0000", "#FF0000", "#FFFFFF", "#00FF00", "#006400"),
#     SM = c("#8B4513", "#D2691E", "#87CEEB", "#4682B4", "#00008B"),
#     CDI = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90")
#   )
#   
#   pals[[indice]] %||% c("#440154", "#31688E", "#35B779", "#FDE724")
# }


# Color breaks configuration for each indice
get_color_config <- function(indice) {
  configs <- list(
    # Precipitation (mm) - 5 classes
    precip = list(
      breaks = c(0, 50, 100, 200, 400, Inf),
      colors = c("#FFFFFF", "#CCEBFF", "#66C2FF", "#0080FF", "#0040CC"),
      labels = c("0-50", "50-100", "100-200", "200-400", ">400")
    ),
    
    # SPI (Standardized Precipitation Index) - 6 classes
    SPI = list(
      breaks = c(-Inf, -2, -1.5, -1, 1, 1.5, 2, Inf),
      colors = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90", "#00BFFF", "#0000FF"),
      labels = c("<-2 (Extrême)", "-2 à -1.5 (Sévère)", "-1.5 à -1 (Modéré)", 
                 "-1 à 1 (Normal)", "1 à 1.5 (Modéré humide)", "1.5 à 2 (Humide)", ">2 (Très humide)")
    ),
    
    # LST (Land Surface Temperature) °C - 5 classes
    LST = list(
      breaks = c(0, 10, 20, 30, 40, 50),
      colors = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000"),
      labels = c("0-10°C", "10-20°C", "20-30°C", "30-40°C", ">40°C")
    ),
    
    # LST Anomaly (°C deviation) - diverging
    LST_A = list(
      breaks = c(-Inf, -3, -1, 1, 3, Inf),
      colors = c("#0000FF", "#87CEEB", "#FFFFFF", "#FFB6C1", "#FF0000"),
      labels = c("<-3 (Froid extrême)", "-3 à -1 (Froid)", "-1 à 1 (Normal)", 
                 "1 à 3 (Chaud)", ">3 (Chaud extrême)")
    ),
    
    # NDVI - 5 classes
    NDVI = list(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      colors = c("#8B4513", "#D2B48C", "#90EE90", "#228B22", "#006400"),
      labels = c("0 à 0.2", "0.2 à 0.4", "0.4 à 0.6", "0.6 à 0.8", "0.8 à 1")
    ),
    
    # ANDVI (Anomaly) - diverging
    ANDVI = list(
      breaks = c(-Inf, -0.3, -0.1, 0.1, 0.3, Inf),
      colors = c("#8B0000", "#FF0000", "#FFFFFF", "#00FF00", "#006400"),
      labels = c("<-0.3 (Stress sévère)", "-0.3 à -0.1 (Stress)", 
                 "-0.1 à 0.1 (Normal)", "0.1 à 0.3 (Santé bonne)", ">0.3 (Excellente santé)")
    ),
    
    # Soil Moisture - 5 classes
    SM = list(
      breaks = c(0, 0.1, 0.2, 0.4, 0.6, 1),
      colors = c("#8B4513", "#D2691E", "#87CEEB", "#4682B4", "#00008B"),
      labels = c("Très sec", "Sec", "Modéré", "Humide", "Très humide")
    ),
    
    # CDI (Combined Drought Index) - 5 classes
    CDI = list(
      breaks = c(-Inf, -3, -2, -1, 0, Inf),
      colors = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90"),
      labels = c("Sécheresse extrême", "Sécheresse sévère", "Sécheresse modérée", 
                 "Anomalie faible", "Conditions normales")
    )
  )
  
  configs[[indice]] %||% list(
    breaks = c(0, 25, 50, 75, 100),
    colors = c("#440154", "#31688E", "#35B779", "#FDE724"),
    labels = c("Très faible", "Faible", "Moyen", "Élevé")
  )
}

# Helper function to get color palette function for leaflet
get_color_palette <- function(indice) {
  config <- get_color_config(indice)
  colorBin(
    palette = config$colors,
    bins = config$breaks,
    na.color = "transparent"
  )
}

# Helper to get legend labels
get_legend_labels <- function(indice) {
  config <- get_color_config(indice)
  config$labels
}

# Helper to get legend colors
get_legend_colors <- function(indice) {
  config <- get_color_config(indice)
  config$colors
}

# Helper to get indice title
get_indice_title <- function(indice) {
  titles <- list(
    precip = "Précipitations (mm)",
    SPI = "Indice SPI",
    LST = "Température (°C)",
    LST_A = "Anomalie Température (°C)",
    NDVI = "NDVI",
    ANDVI = "Anomalie NDVI",
    SM = "Humidité du sol",
    CDI = "Indice de sécheresse"
  )
  titles[[indice]] %||% indice
}