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
get_palette <- function(indice) {
  pals <- list(
    precip = c("#FFFFFF", "#CCEBFF", "#66C2FF", "#0080FF", "#0040CC"),
    SPI = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90", "#00BFFF"),
    LST = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000"),
    LST_A = c("#0000FF", "#FFFFFF", "#FF0000"),
    NDVI = c("#8B4513", "#D2B48C", "#90EE90", "#228B22", "#006400"),
    ANDVI = c("#8B0000", "#FF0000", "#FFFFFF", "#00FF00", "#006400"),
    SM = c("#8B4513", "#D2691E", "#87CEEB", "#4682B4", "#00008B"),
    CDI = c("#8B0000", "#FF0000", "#FFA500", "#FFFF00", "#90EE90")
  )
  
  pals[[indice]] %||% c("#440154", "#31688E", "#35B779", "#FDE724")
}