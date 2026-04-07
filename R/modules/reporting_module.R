# Page 1 Module: Dashboard
source("R/modules/reporting/sidebar_reporting.R")
source("R/modules/reporting/analyse_temporelle_module.R")
source("R/modules/reporting/classification.R")
source("R/modules/reporting/cartes.R")
source("R/modules/reporting/bulletins_module.R")
source("R/modules/reporting/dashboard.R")

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

          classification_ui(ns("classification"))
        ),
        
        # analyse temporelle module
        conditionalPanel(
          condition = "input.reporting_active_module === 'analyse_temporelle'",
          ns = ns,

          analyse_temporelle_ui(ns("analyse"))
        ),
        
        # Dashboard module
        conditionalPanel(
          condition = "input.reporting_active_module === 'dashboard'",
          ns = ns,

          dashboard_ui(ns("dashboard"))
        ),
        
        # Maps module
        conditionalPanel(
          condition = "input.reporting_active_module === 'maps'",
          ns = ns,

          cartes_ui(ns("cartes"))
        ),
        
        # Bulletins module
        conditionalPanel(
          condition = "input.reporting_active_module === 'bulletins'",
          ns = ns,
          bulletinUI(ns("bulletins"))
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
    
    # Bulletins module server
    bulletinsServer("bulletins")
    
    # dashboard server
    dashboard_server("dashboard")
    
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





