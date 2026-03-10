# Classification table
# Build date string matching parse_date() format
build_date_str <- function(temporalite, mois, trimestre, annee) {
  switch(temporalite,
         "mensuel" = {
           mois_fr <- c("janvier", "février", "mars", "avril", "mai", "juin",
                        "juillet", "août", "septembre", "octobre", "novembre", "décembre")
           paste(mois_fr[as.integer(mois)], annee)
         },
         "trimestriel" = paste0(annee, "-T", trimestre),
         "annuel"      = as.character(annee)
  )
}

# Get mask geometry based on niveau
get_mask_geom <- function(niveau, region_detail, province_detail, commune_detail,
                          region_filter, region_commune_filter, province_commune_filter) {
  switch(niveau,
         "National"   = st_union(maroc),
         "Régional"   = st_union(regions[regions$nom_fr == region_detail, ]),
         "Provincial" = st_union(provinces[provinces$Nom_Provinces == province_detail, ]),
         "Communal"   = st_union(communes[communes$commune == commune_detail, ]),
         NULL
  )
}

# Compute class percentages from raster
compute_class_percentages <- function(rast, config) {
  vals <- values(rast)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) return(NULL)
  
  breaks <- config$breaks
  labels <- config$labels
  colors <- config$colors
  
  counts <- sapply(seq_along(labels), function(i) {
    sum(vals >= breaks[i] & vals < breaks[i + 1])
  })
  
  pct <- round(counts / length(vals) * 100, 1)
  
  data.frame(
    Classe     = labels,
    Pixels     = counts,
    Percentage = paste0(pct, "%"),
    stringsAsFactors = FALSE
  )
}