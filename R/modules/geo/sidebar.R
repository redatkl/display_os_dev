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
         icon("droplet", class = "fa-lg"),
         span(class = "icon-tooltip", "Indices de l'eau")
       ),
       
       # Icon 4 - Indice sol
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel4"),
         icon("layer-group", class = "fa-lg"),
         span(class = "icon-tooltip", "Indices de sol")
       ),
       
       # Icon 5 - Indice combinés
       div(
         class = "sidebar-icon",
         `data-panel` = ns("panel5"),
         icon("cog", class = "fa-lg"),
         span(class = "icon-tooltip", "Indices combinés")
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
             div(class = "map-selector-header",
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
         
         # choix d'indice de climat
         div(
           id = ns("indice_climat"),
           class = "indice-section",
           div(class = "indice-header",
               icon("database"), 
               span("Choix d'indice")
           ),
           div(class = "indice-container",
               toggle_switch_group(
                 group_id = ns("filter_climate_options"),
                 options = list(
                   "precip" = "Précipitations (Chiprs)",
                   "SPI" = "Indice de précipitations standardisé (SPI)", 
                   "LST" = "Température de surface terrestre (LST)",
                   "LST_A" = "Anomalie de température de surface terrestre"
                 ),
                 selected = "precip"
               )
               
           )),
         # la zone de la légende
         div(
           id = ns("legend"),
           class = "legend-section",
           div(class = "legend-header",
               icon("circle-info"), 
               span("Légende")
           ),
           div(class = "legend-container",
               # Placeholder for legend content
               tags$img(src = "logos/logo.png", 
                        height = "100px",
                        width = "150px",
                        alt = "Légende des indices climatiques", 
                        class = "legend-image"))
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
      map1 = list(temporalite = "mensuel", date = "décembre 2025", date_raw = Sys.Date()),
      map2 = list(temporalite = "mensuel", date = "décembre 2025", date_raw = Sys.Date()),
      map3 = list(temporalite = "mensuel", date = "décembre 2025", date_raw = Sys.Date()),
      map4 = list(temporalite = "mensuel", date = "décembre 2025", date_raw = Sys.Date())
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
    
    # Function to restore map parameters
    restoreMapParameters <- function(map_id) {
      saved_params <- map_params[[map_id]]
      
      cat("Restoring parameters for", map_id, "\n")
      cat(" Indice: ")
      cat("  Temporalite:", saved_params$temporalite, "\n")
      cat("  Date:", saved_params$date, "\n")
      
      # Update toggle switch
      session$sendCustomMessage(
        type = "updateToggleSwitch",
        message = list(
          id = session$ns("filter_options"),
          value = saved_params$temporalite
        )
      )
      
      # Update date picker with the already formatted date string
      session$sendCustomMessage(
        type = "updateDatePickerValue",
        message = list(
          id = session$ns("custom_date"),
          date = saved_params$date,
          temporalite = saved_params$temporalite
        )
      )
    }
    
    # Observer for the map layout 
    observeEvent(input$map_layout_selected, {
      cat("Map layout changed to:", input$map_layout_selected, "\n")
      # Reset to map1 when layout changes
      active_map("map1")
      
      # Small delay to ensure UI is ready
      invalidateLater(100, session)
      
      # Restore map1 parameters after UI is ready
      isolate({
        restoreMapParameters("map1")
      })
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Observer for active map selector
    observeEvent(input$active_map_selector, {
      req(input$active_map_selector)
      cat("Active map changed to:", input$active_map_selector, "\n")
      active_map(input$active_map_selector)
      
      # Small delay to ensure the active_map() has updated
      invalidateLater(100, session)
      
      # Restore saved parameters for this map
      isolate({
        restoreMapParameters(input$active_map_selector)
      })
    }, ignoreInit = TRUE)
    
    # Observer for temporality changes
    observeEvent(input$filter_options, {
      req(active_map())
      
      current_map <- active_map()
      
      # Save temporality to current active map
      map_params[[current_map]]$temporalite <- input$filter_options
      
      # Send the new temporalite to JavaScript
      session$sendCustomMessage(
        type = "updateDatePickerTemporalite",
        message = list(
          id = session$ns("custom_date"),
          temporalite = input$filter_options
        )
      )
      
      cat("Map:", current_map, "- Selected temporality:", input$filter_options, "\n")
    }, ignoreInit = TRUE)
    
    # Observer for date changes
    observeEvent(input$custom_date, {
      req(active_map())
      
      current_map <- active_map()
      
      # Save the formatted date string directly
      map_params[[current_map]]$date <- input$custom_date
      
      cat("Map:", current_map, "- Selected date:", input$custom_date, "\n")
    }, ignoreInit = TRUE)
    
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
      date_range = reactive(input$date_range),
      map_params = map_params,
      active_map = active_map
    ))
  })
}