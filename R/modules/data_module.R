# Page 2 Module: Data

data_ui <- function(id) {
  ns <- NS(id)
  
  indices <- list(
    list(id = "spi",      icon = "☁️️", label = "SPI",          sub ="Indice de précipitations standardisé"),
    list(id = "andvi",    icon = "🌿", label = "ANDVI",         sub = "Anomalie de l'indice de végétation"),
    list(id = "alst",     icon = "🌡️", label = "ALST",          sub = "Anomalie de surface terrestre"),
    list(id = "compo",    icon = "☰️", label = "Composite",     sub = "Indice combiné"),
    list(id = "soilmois", icon = "💧", label = "Soil Moisture", sub = "Anomalie d'humidité de sol"),
    list(id = "action",   icon = "⚠", label = "Action",        sub = "TIndice d'action")
  )
  
  make_card <- function(idx, active = FALSE) {
    div(
      class = paste("index-card", if (active) "active"),
      id    = ns(paste0("card_", idx$id)),
      # onclick sends the indice id to Shiny
      onclick = sprintf(
        "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
        ns("selected_indice"), idx$id
      ),
      div(class = "index-icon", idx$icon),
      div(
        tags$p(class = "index-label",    idx$label),
        tags$p(class = "index-sublabel", idx$sub)
      )
    )
  }
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/data_module.css"),
      tags$script(src = "js/data_module.js")
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
        uiOutput(ns("pdf_viewer"))
      )
    )
  )
}

data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Map indice id → PDF filename (place files in www/pdf/)
    pdf_map <- list(
      spi      = "SPI.pdf",
      andvi    = "ANDVI.pdf",
      alst     = "ALST.pdf",
      compo    = "Composite.pdf",
      soilmois = "ASMI.pdf",
      action   = "Action.pdf"
    )
    
    selected_indice <- reactiveVal("spi")
    
    # Active card highlight: remove/add .active class via JS
    observeEvent(input$selected_indice, {
      selected_indice(input$selected_indice)
      session$sendCustomMessage("setActiveCard", list(
        ns     = ns(""),
        active = input$selected_indice
      ))
    }, ignoreInit = TRUE)
    
    # Render PDF viewer
    output$pdf_viewer <- renderUI({
      indice <- selected_indice()
      pdf_file <- pdf_map[[indice]]
      src_path <- file.path("www/data_pdf", pdf_file)
      
      if (!file.exists(src_path)) {
        return(
          div(
            style = "padding:40px; color:#c0392b;",
            icon("circle-exclamation"),
            tags$p(paste("Fichier PDF introuvable :", pdf_file))
          )
        )
      }
      
      tags$iframe(
        src         = paste0("data_pdf/", pdf_file),
        style       = "width:100%; height:100%; border:none; flex:1;",
        frameborder = "0"
      )
    })
  })
}