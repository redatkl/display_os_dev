# Page 1 Module: Dashboard
source("R/modules/reporting/sidebar_reporting.R")
source("R/modules/reporting/analyse_temporelle_module.R")
source("R/modules/reporting/classification.R")
source("R/modules/reporting/cartes.R")

reporting_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    sidebar_reporting_ui(ns("sidebar3")),
    
    # Inline script: redirect the bare JS input to the namespaced one
    # so conditionalPanel (which uses the namespaced input) works correctly.
    tags$script(HTML(sprintf("
      $(document).on('shiny:connected', function() {
        // Set namespaced default so conditionalPanel evaluates correctly
        Shiny.setInputValue('%s', 'classification');
      });

      // Intercept every icon click and write to the NAMESPACED input
      $(document).on('click', '.reporting-sidebar .sidebar-reporting-icon', function() {
        var mod = $(this).data('module');
        if (mod) Shiny.setInputValue('%s', mod, { priority: 'event' });
      });
    ", ns("reporting_active_module"), ns("reporting_active_module")))),
    
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
          classification_ui(ns("classification"))
        ),
        
        # analyse temporelle module
        conditionalPanel(
          condition = "input.reporting_active_module === 'analyse_temporelle'",
          ns = ns,
          # ← replace with your actual dashboard module UI
          analyse_temporelle_ui(ns("analyse"))
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
          cartes_ui(ns("cartes"))
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
    
    # analyse module server
    analyse_temporelle_server("analyse")
    
    # Classification module server
    classification_server("classification")
    
    # Cartes module server
    cartes_server("cartes")
    
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





