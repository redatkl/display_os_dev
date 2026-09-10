# sidebar module in the page stations
source("R/functions/station_panel_component.R")

sidebar_stations_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar_stations.css"),
      tags$script(src = "js/sidebar_stations.js"),
      tags$script(src = "js/toggle_button.js"),
      tags$script(src = "js/datepicker.js")
    ),
    
    div(
      id = ns("sidebar-container"),
      class = "custom-sidebar stations-sidebar",
      
      # Icon buttons
      div(
        class = "sidebar-icons",
        
        div(
          class = "sidebar-icon active",
          #`data-panel` = ns("source_panel"),
          icon("database"),
          span(class = "icon-tooltip", "Source de données")
        ),
        
        div(
          class = "sidebar-icon",
          #`data-panel` = ns("variable_panel"),
          icon("chart-line"),
          span(class = "icon-tooltip", "Variables météorologiques")
        ),
        
        div(
          class = "sidebar-icon",
          #`data-panel` = ns("periode_panel"),
          icon("calendar"),
          span(class = "icon-tooltip", "Choix de temporalité")
        )
      ),
      
      # Stations sidebar panel
      div(
        class = "station-sidebar-panels",

        div(
          id = ns("stations_panel"),
          class = "station-sidebar-panel",
        
        
        # panel 1 - source de données
            create_station_panel_content(
            ns = ns,
            panel_id = "source",
            options = list(
              "station_phys" = "Stations Physiques",
              "station_virt" = "Stations Virtuelles"
            ),
            selected = "station_phys",
            label = "Source de données",
            include_button = FALSE
          ),
          
 # Panel 2 - Variables météorologiques
          create_station_panel_content(
            ns = ns,
            panel_id = "variable",
            options = list(
              "temp"   = "Température",
              "precip" = "Précipitations"
            ),
            selected = "temp",
            label = "Variables météorologiques",
            include_button = FALSE
          ),
        
        # Panel 3 - Choix de temporalité
          create_station_panel_content(
            ns = ns,
            panel_id = "periode",
            include_temporalite = TRUE,
            include_button = TRUE
          )
        )
      )
      )
    )
}


# sidebar stations server
sidebar_stations_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store user selections
    station_params <- reactiveValues(
      data_source = "station_phys",
      variable = "temp",
      date = Sys.Date(),
      update_trigger = 0
    )
    
    # Observer for data source
    observeEvent(input$filter_source_options, {
      station_params$data_source <- input$filter_source_options
      cat("Data source changed to:", input$filter_source_options, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for variable
    observeEvent(input$filter_variable_options, {
      station_params$variable <- input$filter_variable_options
      cat("Variable changed to:", input$filter_variable_options, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for date
    observeEvent(input$filter_options_periode, {
      station_params$date <- input$filter_options_periode
      cat("Date changed to:", input$filter_options_periode, "\n")
    }, ignoreInit = TRUE)
    
  observeEvent(input$custom_date_periode, {
        station_params$date <- input$custom_date_periode
        cat("Date changed to:", input$custom_date_periode, "\n")
      }, ignoreInit = TRUE)

    # Observer for update button
    observeEvent(input$update_periode, {
      station_params$update_trigger <- station_params$update_trigger + 1
      cat("Update button clicked - Trigger:", station_params$update_trigger, "\n")
      cat("Current params: source =", station_params$data_source, 
           ", variable =", station_params$variable,
          ", temporalite =", station_params$temporalite,
          ", date =", station_params$date, "\n")
    }, ignoreInit = TRUE)
    
    # Return reactive values
    return(station_params)
  })
}