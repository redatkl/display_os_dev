# Sidebar module for the page forecast

sidebar_forecast_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar_forecast.css"),
      tags$script(src = "js/sidebar_forecast.js")
    ),
    
    div(
      id = ns("sidebar-container"),
      class = "custom-sidebar forecast-sidebar",
      
      # Icon buttons
      div(
        class = "sidebar-icons",
        
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
        class = "forecast-panel-wrapper expanded",
        div(
          class = "forecast-panel active",
          
          # Block 1: Variable
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
          
          # Block 2: Période
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
                      min = Sys.Date(),
                      max = Sys.Date() + 14,
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
              ns("update_forecast_chart"),
              label = "Mettre à jour prévisions",
              class = "btn-update-forecast",
              icon = icon("refresh")
            )
          )
        )
      )
    )
  )
}


# server side forecast module
sidebar_forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store user selections
    forecast_vals <- reactiveValues(
      variable = "temp",
      date = Sys.Date(),
      update_trigger = 0
    )
    
    # Observe variable selection
    observeEvent(input$variable, {
      forecast_vals$variable <- input$variable
    })
    
    # Observe date selection
    observeEvent(input$selected_date, {
      forecast_vals$date <- input$selected_date
    })
    
    # Observe update button click
    observeEvent(input$update_station_chart, {
      forecast_vals$update_trigger <- forecast_vals$update_trigger + 1
    })
    
    return(list(
      variable = reactive(forecast_vals$variable),
      date = reactive(forecast_vals$date),
      update_trigger = reactive(forecast_vals$update_trigger)
    ))
  })
}
