# Page 2 Module: Data

data_ui <- function(id) {
  ns <- NS(id)
  
  indices <- list(
    list(id = "spi",      icon = "☁️️", label = "SPI",          sub =""),
    list(id = "andvi",    icon = "🌿", label = "ANDVI",         sub = "Anomaly of NDVI"),
    list(id = "alst",     icon = "🌡️", label = "ALST",          sub = "Anomaly of LST"),
    list(id = "compo",    icon = "☰️", label = "Composite",     sub = "Combined Drought Index"),
    list(id = "soilmois", icon = "💧", label = "Soil Moisture", sub = "Anomalie d'humidité"),
    list(id = "action",   icon = "⚠", label = "Action",        sub = "Threshold & Alarms")
  )
  
  make_card <- function(idx, active = FALSE) {
    div(
      class = paste("index-card", if (active) "active"),
      id    = ns(paste0("card_", idx$id)),
      div(class = "index-icon", idx$icon),
      div(
        tags$p(class = "index-label",    idx$label),
        tags$p(class = "index-sublabel", idx$sub)
      )
    )
  }
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/data_module.css")
    ),
    
    # div of the config panel
    div(
      class = "config-panel-wrapper",
      
      # ── Left sidebar ──────────────────────────────
      div(
        class = "config-sidebar",
        
        div(class = "title-data", tags$p("Data indices")),
      
        div(class = "title-descr", tags$p("Sélectionnez un indice pour consulter la documentation.")),
        
        # Cards (first one active by default)
        make_card(indices[[1]], active = TRUE),
        lapply(indices[-1], make_card)
      ),
      
      # ── Right panel (doc viewer placeholder) ──────
      div(
        class = "doc-panel",
        style = "flex:1; padding: 40px;"
        
      )
    )
  )
}

data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

  })
}