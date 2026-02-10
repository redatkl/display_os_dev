# sidebar module in the page stations

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
          `data-block` = "source",
          icon("database"),
          span(class = "icon-tooltip", "Source de données")
        ),
        
        div(
          class = "sidebar-icon",
          `data-block` = "variable",
          icon("chart-line"),
          span(class = "icon-tooltip", "Variable")
        ),
        
        div(
          class = "sidebar-icon",
          `data-block` = "periode",
          icon("calendar"),
          span(class = "icon-tooltip", "Période")
        )
      ),
      
      # Panel wrapper
      div(
        class = "station-panel-wrapper expanded",
        div(
          class = "station-panel active",
          
          # Block 1: Source
          div(
            id = ns("source_block"),
            class = "data-block active",
            `data-block` = "source",
            div(class = "block-title",
                icon("database"), 
                span("Source de données")
            ),
            div(class = "block-body",
                toggle_switch_group(
                  group_id = ns("data_source"),
                  options = list("station1" = "Station 1", "station2" = "Station 2"),
                  selected = "station1"
                )
            )
          ),
          
          # Block 2: Variable
          div(
            id = ns("variable_block"),
            class = "data-block",
            `data-block` = "variable",
            div(class = "block-title",
                icon("chart-line"), 
                span("Variable")
            ),
            div(class = "block-body",
                toggle_switch_group(
                  group_id = ns("variable"),
                  options = list("temp" = "Température", "precip" = "Précipitations"),
                  selected = "temp"
                )
            )
          ),
          
          # Block 3: Période
          div(
            id = ns("periode_block"),
            class = "data-block",
            `data-block` = "periode",
            div(class = "block-title",
                icon("calendar"), 
                span("Période")
            ),
            div(class = "block-body",
                div(class = "date-input-group",
                    tags$label("Sélectionner une date", class = "date-label"),
                    dateInput(
                      ns("selected_date"),
                      label = NULL,
                      value = Sys.Date(),
                      max = Sys.Date(),
                      format = "dd/mm/yyyy",
                      language = "fr",
                      weekstart = 1
                    )
                )
            )
          ),
          
          # Update the map button
          div(
            class = "update-button-container",
            actionButton(
              ns("update_station_chart"),
              label = "Mettre à jour stations",
              class = "btn-update-station",
              icon = icon("refresh")
            )
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
      data_source = "station1",
      variable = "temp",
      date = Sys.Date(),
      update_trigger = 0
    )
    
    # Observer for data source
    observeEvent(input$data_source, {
      station_params$data_source <- input$data_source
      cat("Data source changed to:", input$data_source, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for variable
    observeEvent(input$variable, {
      station_params$variable <- input$variable
      cat("Variable changed to:", input$variable, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for date
    observeEvent(input$selected_date, {
      station_params$date <- input$selected_date
      cat("Date changed to:", input$selected_date, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for update button
    observeEvent(input$update_station_chart, {
      station_params$update_trigger <- station_params$update_trigger + 1
      cat("Update button clicked - Trigger:", station_params$update_trigger, "\n")
      cat("Current params: source =", station_params$data_source, 
          ", variable =", station_params$variable, 
          ", date =", as.character(station_params$date), "\n")
    }, ignoreInit = TRUE)
    
    # Return reactive values
    return(station_params)
  })
}