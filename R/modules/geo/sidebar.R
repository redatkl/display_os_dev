# the sidebar module 
source("R/functions/toggle_button.R")




sidebarModuleUI <- function(id) {
 ns <- NS(id)
 
 tagList(
   # Include custom CSS and JS
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar.css"),
     tags$link(rel = "stylesheet", type = "text/css", href = "css/toggle_button.css"),
     tags$script(src = "js/sidebar.js"),
     tags$script(src = "js/toggle_button.js")
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
       )
     ),
     
     # Collapsible panels
     div(
       class = "sidebar-panels",
       
       # Panel 1 - Dashboard
       div(
         id = ns("panel1"),
         class = "sidebar-panel",
         # choix de la temporalité des indices (mensuelles, annuelle ...)
         div(
           id = ns("temporalite"),
           class = "temporalite-section",
           div(class = "header",
               icon("calendar"), 
               span("Choix de temporalité")
           ),
           div(class = "temporalite-container",
               toggle_switch("decadaire", "Décadaire"),
               toggle_switch("mensuel", "Mensuel"),
               toggle_switch("trimestriel", "Trimestriel"),
               toggle_switch("annuel", "Annuel")
           ),
           div(class = "date-input",
               dateInput(ns("date_selection"), 
                         label = NULL,
                         value = Sys.Date())
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
         toggle_switch("feature_a", "Feature A"),
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
    
    # Dashboard button click
    observeEvent(input$dashboard_btn, {
      showNotification("Dashboard updated!", type = "success")
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

