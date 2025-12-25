# the sidebar module 
source("R/functions/toggle_button.R")
source("R/functions/datepicker.R")




sidebarModuleUI <- function(id) {
 ns <- NS(id)
 
 tagList(
   # Include custom CSS and JS
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar.css"),
     tags$link(rel = "stylesheet", type = "text/css", href = "css/toggle_button.css"),
     tags$link(rel = "stylesheet", type = "text/css", href = "css/datepicker.css"),
     tags$script(src = "js/sidebar.js"),
     tags$script(src = "js/toggle_button.js"),
     tags$script(src = "js/datepicker.js")
   ),
   
   # Sidebar container
   div(
     id = ns("sidebar-container"),
     class = "custom-sidebar",
     
     # Icon buttons
     div(
       class = "sidebar-icons",
       
       # Icon 1 - indices climat
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel1"),
         icon("cloud", class = "fa-solid fa-cloud"),
         span(class = "icon-tooltip", "Indices Climat")
       ),
       
       # Icon 2 - indices végétaion
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel2"),
         icon("leaf", class = "fa-solid fa-leaf"),
         span(class = "icon-tooltip", "Indices de Végétation")
       ),
       
       # Icon 3 - Analytics
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel3"),
         icon("chart-line", class = "fa-lg"),
         span(class = "icon-tooltip", "Analytics")
       ),
       
       # Icon 4 - User
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel4"),
         icon("user", class = "fa-lg"),
         span(class = "icon-tooltip", "Profile")
       ),
       
       # Icon 5 - Settings
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel5"),
         icon("cog", class = "fa-lg"),
         span(class = "icon-tooltip", "Settings")
       ),
       
       # split panel icons with a line and the icons for maps layout
       div(class = "sidebar-divider"),
       
       # Icon 6 - Map layout 1
       div(
         class = "map-layout-icon",
         `data-map-layout` = "layout1",
         `data-input-id` = ns("map_layout_selected"), 
         icon("square", class = "fa-lg"),
         span(class = "icon-tooltip", "1 carte")
       ),
       
       # Icon 7 - Map layout 2
       div(
         class = "map-layout-icon",
         `data-map-layout` = "layout2",
         `data-input-id` = ns("map_layout_selected"),  
         icon("columns", class = "fa-lg"),
         span(class = "icon-tooltip", "2 cartes")
       ),
       
       # Icon 8 - Map layout 4
       div(
         class = "map-layout-icon",
         `data-map-layout` = "layout4",
         `data-input-id` = ns("map_layout_selected"), 
         icon("th-large", class = "fa-lg"),
         span(class = "icon-tooltip", "4 cartes")
       )
     ),
     
     # Collapsible panels
     div(
       class = "sidebar-panels",
       
       # Panel 1 - Dashboard
       div(
         id = ns("panel1"),
         class = "sidebar-panel",
         
         # Map selector (conditionally shown)
         conditionalPanel(
           condition = "input.map_layout_selected == 'layout2' || input.map_layout_selected == 'layout4'",
           ns = ns,
           div(
             class = "map-selector-container",
             div(class = "header",
                 icon("map"), 
                 span("Carte active")
             ),
             uiOutput(ns("map_selector_ui"))
           )
         ),
         
         # choix de la temporalité des indices (mensuelles, annuelle ...)
         div(
           id = ns("temporalite"),
           class = "temporalite-section",
           div(class = "header",
               icon("calendar"), 
               span("Choix de temporalité")
           ),
           div(class = "temporalite-container",
               toggle_switch_group(
                 group_id = ns("filter_options"),
                 options = list(
                   "decadaire" = "Décadaire",
                   "mensuel" = "Mensuel", 
                   "trimestriel" = "Trimestriel",
                   "annuel" = "Annuel"
                 ),
                 selected = "mensuel"
               )

           ),
           div(class = "date-input",
               customDatePickerInput(ns("custom_date"), value = Sys.Date())
           )
         ),
         
         # choix de l'indice en question
         div(
           id = ns("indice"),
           class = "indice-section",
           
         ),
         # la zone de la légende
         div(
           id = ns("legend"),
           class = "legend-section",
           customDatePickerInput("test_date", "Choose a date:", value = Sys.Date())
         )
       ),
       
       # Panel 2 - Settings
       div(
         id = ns("panel2"),
         class = "sidebar-panel",
         h4("Settings"),
         checkboxInput(ns("setting1"), "Enable notifications", TRUE),
         checkboxInput(ns("setting2"), "Auto-refresh", FALSE),
         radioButtons(ns("theme"), "Theme:", 
                      choices = c("Light", "Dark", "Auto")),
         actionButton(ns("save_settings"), "Save Settings", 
                      class = "btn-success btn-sm")
       ),
       
       # Panel 3 - Analytics
       div(
         id = ns("panel3"),
         class = "sidebar-panel",
         h4("Analytics Options"),
         dateRangeInput(ns("date_range"), "Date Range:"),
         checkboxGroupInput(ns("metrics"), "Metrics:",
                            choices = c("Views", "Clicks", "Conversions")),
         actionButton(ns("generate_report"), "Generate Report", 
                      class = "btn-info btn-sm")
       ),
       
       # Panel 4 - User Profile
       div(
         id = ns("panel4"),
         class = "sidebar-panel",
         h4("User Profile"),
         #toggle_switch("feature_a", "Feature A"),
         textInput(ns("username"), "Username:"),
         textInput(ns("email"), "Email:"),
         selectInput(ns("role"), "Role:", 
                     choices = c("Admin", "Editor", "Viewer")),
         actionButton(ns("update_profile"), "Update Profile", 
                      class = "btn-warning btn-sm")
       )
     )
   ),
   
   # Main content area
   #div(
     #class = "main-content-area",
      #h4("Your main content goes here")
   #)
 )
}

# Module Server
sidebarModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store parameters for each map
    map_params <- reactiveValues(
      map1 = list(temporalite = "mensuel", date = Sys.Date()),
      map2 = list(temporalite = "mensuel", date = Sys.Date()),
      map3 = list(temporalite = "mensuel", date = Sys.Date()),
      map4 = list(temporalite = "mensuel", date = Sys.Date())
    )
    
    # Current active map
    active_map <- reactiveVal("map1")
    
    # Render map selector based on layout
    output$map_selector_ui <- renderUI({
      req(input$map_layout_selected)
      
      map_choices <- switch(input$map_layout_selected,
                            "layout1" = c("Carte 1" = "map1"),
                            "layout2" = c("Carte 1" = "map1", "Carte 2" = "map2"),
                            "layout4" = c("Carte 1" = "map1", "Carte 2" = "map2", 
                                          "Carte 3" = "map3", "Carte 4" = "map4"),
                            c("Carte 1" = "map1")
      )
      
      selectizeInput(
        session$ns("active_map_selector"),
        label = NULL,
        choices = map_choices,
        selected = active_map(),
        width = "100%",
        options = list(
          placeholder = "Sélectionner une carte"
        )
      )
    })
    
   # Observer for the map layout 
    observeEvent(input$map_layout_selected, {
      cat("Map layout changed to:", input$map_layout_selected, "\n")
      # Reset to map1 when layout changes
      active_map("map1")
    }, ignoreNULL = TRUE)
    
    
    # Observer for active map selector
    observeEvent(input$active_map_selector, {
      req(input$active_map_selector)
      cat("Active map changed to:", input$active_map_selector, "\n")
      active_map(input$active_map_selector)
      
      # Load saved parameters for this map
      saved_params <- map_params[[input$active_map_selector]]
      
      # Update the UI with saved values
      # Note: You'll need to trigger updates to your custom inputs here
      session$sendCustomMessage(
        type = "updateDatePickerTemporalite",
        message = list(
          id = session$ns("custom_date"),
          temporalite = saved_params$temporalite
        )
      )
    })
    
    
    # display the switch toggle choice
    observeEvent(input$filter_options, {
      
      req(active_map())
      
      # Save to current active map
      map_params[[active_map()]]$temporalite <- input$filter_options
      
      temporalite <- input$filter_options
      # Send the new temporalite to JavaScript
      session$sendCustomMessage(
        type = "updateDatePickerTemporalite",
        message = list(
          id = session$ns("custom_date"),  # datepicker input ID
          temporalite = temporalite
        )
      )
      print(paste("Map:", active_map(), "- Selected temporality:", input$filter_options))
    })
    
    # display the date chosen
    observeEvent(input$custom_date, {
      
      req(active_map())
     # Save to current active map
      map_params[[active_map()]]$date <- input$custom_date
      
      print(paste("Map:", active_map(), "- Selected date:", input$custom_date))
    })
    
    # Settings save
    observeEvent(input$save_settings, {
      showNotification("Settings saved!", type = "success")
    })
    
    # Report generation
    observeEvent(input$generate_report, {
      showNotification("Report generated!", type = "info")
    })
    
    # Profile update
    observeEvent(input$update_profile, {
      showNotification("Profile updated!", type = "warning")
    })
    
    # Return values that can be used by the main app
    return(list(
      dashboard_select = reactive(input$dashboard_select),
      theme = reactive(input$theme),
      date_range = reactive(input$date_range)
    ))
  })
}

