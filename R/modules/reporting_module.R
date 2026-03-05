# Page 1 Module: Dashboard
source("R/modules/reporting/sidebar_reporting.R")

reporting_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    sidebar_reporting_ui(ns("sidebar3")),
    
    # Main content — full area to the right of the icon strip
    div(
      class = "reporting-main-content",
      div(
        class = "reporting-main-wrapper",
        
        # Classification module (also the default)
        conditionalPanel(
          condition = "typeof input.reporting_active_module === 'undefined' || input.reporting_active_module === '' || input.reporting_active_module === 'classification'",
          ns = ns,
          # ← replace with your actual classification module UI
          tags$h3("Classification", style = "padding:20px;")
        ),
        
        # Dashboard module
        conditionalPanel(
          condition = "input.reporting_active_module === 'dashboard'",
          ns = ns,
          # ← replace with your actual dashboard module UI
          tags$h3("Tableau de bord", style = "padding:20px;")
        ),
        
        # Maps module
        conditionalPanel(
          condition = "input.reporting_active_module === 'maps'",
          ns = ns,
          # ← replace with your actual maps module UI
          tags$h3("Cartes", style = "padding:20px;")
        ),
        
        # Bulletins module
        conditionalPanel(
          condition = "input.reporting_active_module === 'bulletins'",
          ns = ns,
          # ← replace with your actual bulletins module UI
          tags$h3("Bulletins", style = "padding:20px;")
        )
      )
    )
  )
}

reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialise sidebar and get back the active_module reactive
    sidebar_vals <- sidebar_reporting_server("sidebar3")
    
    # React to module changes — add per-module server logic here
    observeEvent(input$reporting_active_module, {
      cat("[Reporting] Active module:", input$reporting_active_module, "\n")
      
      # e.g.:
      # if (input$reporting_active_module == "bulletins") {
      #   # initialise bulletin server logic
      # }
    })
  })
}





