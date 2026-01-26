# the sidebar module 
source("R/functions/toggle_button.R")
source("R/functions/datepicker.R")
source("R/functions/panel_component.R")

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
          
          create_panel_content(
            ns = ns,
            panel_id = "climate",
            indices_options = list(
              "precip" = "Précipitations (Chiprs)",
              "SPI" = "Indice de précipitations standardisé (SPI)", 
              "LST" = "Température de surface terrestre (LST)",
              "LST_A" = "Anomalie de température de surface terrestre"
            ),
            indices_label = "Choix d'indice climat"
          )
        ),
        
        # Panel 2 - indice de végétation
        div(
          id = ns("panel2"),
          class = "sidebar-panel",
          create_panel_content(
            ns = ns,
            panel_id = "vegetation",
            indices_options = list(
              "NDVI" = "NDVI - Indice de végétation normalisé",
              "EVI" = "EVI - Indice de végétation amélioré",
              "SAVI" = "SAVI - Indice de végétation ajusté au sol",
              "LAI" = "LAI - Indice de surface foliaire"
            ),
            indices_label = "Choix d'indice de végétation"
          )
        ),
        
        # Panel 3 - indice de l'eau
        div(
          id = ns("panel3"),
          class = "sidebar-panel",
          create_panel_content(
            ns = ns,
            panel_id = "water",
            indices_options = list(
              "NDWI" = "NDWI - Indice d'eau normalisé"
            ),
            indices_label = "Choix d'indice de l'eau"
          )
        ),
        
        # Panel 4 - indice de sol
        div(
          id = ns("panel4"),
          class = "sidebar-panel",
          create_panel_content(
            ns = ns,
            panel_id = "soil",
            indices_options = list(
              "SM" = "Indice d'humidité de sol"
            ),
            indices_label = "Choix d'indice de sol"
          )
        ),
        
        # Panel 5 - Indices combonés
        div(
          id = ns("panel5"),
          class = "sidebar-panel",
          create_panel_content(
            ns = ns,
            panel_id = "combined",
            indices_options = list(
              "VCI" = "VCI - Condition de végétation",
              "TCI" = "TCI - Condition de température",
              "VHI" = "VHI - Indice de santé de végétation",
              "SMCI" = "SMCI - Indice de condition d'humidité du sol"
            ),
            indices_label = "Choix d'indice combiné"
          )
        )
      ),
    )
  )
}

# Module Server
sidebarModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store parameters for each map
    map_params <- reactiveValues(
      map1 = list(
        climate = list(temporalite = "mensuel", date = "décembre 2025", indice = "precip", update_trigger = 1),
        vegetation = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDVI", update_trigger = 1),
        water = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDWI", update_trigger = 1),
        soil = list(temporalite = "mensuel", date = "décembre 2025", indice = "SM", update_trigger = 1),
        combined = list(temporalite = "mensuel", date = "décembre 2025", indice = "VCI", update_trigger = 1)
      ),
      map2 = list(
        climate = list(temporalite = "mensuel", date = "décembre 2025", indice = "precip", update_trigger = 1),
        vegetation = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDVI", update_trigger = 1),
        water = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDWI", update_trigger = 1),
        soil = list(temporalite = "mensuel", date = "décembre 2025", indice = "SM", update_trigger = 1),
        combined = list(temporalite = "mensuel", date = "décembre 2025", indice = "VCI", update_trigger = 1)
      ),
      map3 = list(
        climate = list(temporalite = "mensuel", date = "décembre 2025", indice = "precip", update_trigger = 1),
        vegetation = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDVI", update_trigger = 1),
        water = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDWI", update_trigger = 1),
        soil = list(temporalite = "mensuel", date = "décembre 2025", indice = "SM", update_trigger = 1),
        combined = list(temporalite = "mensuel", date = "décembre 2025", indice = "VCI", update_trigger = 1)
      ),
      map4 = list(
        climate = list(temporalite = "mensuel", date = "décembre 2025", indice = "precip", update_trigger = 1),
        vegetation = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDVI", update_trigger = 1),
        water = list(temporalite = "mensuel", date = "décembre 2025", indice = "NDWI", update_trigger = 1),
        soil = list(temporalite = "mensuel", date = "décembre 2025", indice = "SM", update_trigger = 1),
        combined = list(temporalite = "mensuel", date = "décembre 2025", indice = "VCI", update_trigger = 1)
      )
    )
    
    # Current active map for each panel
    active_map_per_panel <- reactiveValues(
      climate = "map1",
      vegetation = "map1",
      water = "map1",
      soil = "map1",
      combined = "map1"
    )
    
    # Current active panel
    active_panel <- reactiveVal("climate")
    
    # Observer to track which panel is currently active
    observeEvent(input$active_panel_type, {
      req(input$active_panel_type)
      
      new_panel <- input$active_panel_type
      active_panel(new_panel)
      
      # Get which map is active for this panel
      current_map <- active_map_per_panel[[new_panel]]
      
      cat("Panel switched to:", new_panel, "- Active map:", current_map, "\n")
      
      # Small delay to ensure UI is rendered
      invalidateLater(100, session)
      
      # Restore this map's parameters for this panel
      isolate({
        restoreMapParameters(current_map, new_panel)
      })
    }, ignoreInit = TRUE)
    
    # Map panel IDs to their types
    panel_types <- list(
      panel1 = "climate",
      panel2 = "vegetation", 
      panel3 = "water",
      panel4 = "soil",
      panel5 = "combined"
    )
    
    # Generate map selector UI for each panel
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      output[[paste0("map_selector_ui_", panel_type)]] <- renderUI({
        req(input$map_layout_selected)
        
        map_choices <- switch(input$map_layout_selected,
                              "layout1" = c("Carte 1" = "map1"),
                              "layout2" = c("Carte 1" = "map1", "Carte 2" = "map2"),
                              "layout4" = c("Carte 1" = "map1", "Carte 2" = "map2", 
                                            "Carte 3" = "map3", "Carte 4" = "map4"),
                              c("Carte 1" = "map1")
        )
        
        # Get the currently active map for THIS panel
        current_active_map <- active_map_per_panel[[panel_type]]
        
        # Ensure the selected map is valid for the current layout
        if (!current_active_map %in% map_choices) {
          current_active_map <- "map1"
          active_map_per_panel[[panel_type]] <- "map1"
        }
        
        selectizeInput(
          session$ns(paste0("active_map_selector_", panel_type)),
          label = NULL,
          choices = map_choices,
          selected = current_active_map,
          width = "100%",
          options = list(
            placeholder = "Sélectionner une carte"
          )
        )
      })
    })
    
    # Function to restore map parameters for a specific panel
    restoreMapParameters <- function(map_id, panel_type) {
      saved_params <- map_params[[map_id]][[panel_type]]
      
      cat("Restoring parameters for", map_id, "in panel", panel_type, "\n")
      cat("  Temporalite:", saved_params$temporalite, "\n")
      cat("  Date:", saved_params$date, "\n")
      cat("  Indice:", saved_params$indice, "\n")
      
      # Update toggle switch for temporality
      session$sendCustomMessage(
        type = "updateToggleSwitch",
        message = list(
          id = session$ns(paste0("filter_options_", panel_type)),
          value = saved_params$temporalite
        )
      )
      
      # Update date picker
      session$sendCustomMessage(
        type = "updateDatePickerValue",
        message = list(
          id = session$ns(paste0("custom_date_", panel_type)),
          date = saved_params$date,
          temporalite = saved_params$temporalite
        )
      )
      
      # Update indice toggle if saved
      if (!is.null(saved_params$indice)) {
        session$sendCustomMessage(
          type = "updateToggleSwitch",
          message = list(
            id = session$ns(paste0("filter_", panel_type, "_options")),
            value = saved_params$indice
          )
        )
      }
    }
    
    # Observer for the map layout 
    observeEvent(input$map_layout_selected, {
      cat("Map layout changed to:", input$map_layout_selected, "\n")
      
      # Get current active panel
      current_panel <- active_panel()
      
      # Reset this panel's active map to map1
      active_map_per_panel[[current_panel]] <- "map1"
      
      invalidateLater(100, session)
      isolate({
        current_panel <- active_panel()
        restoreMapParameters("map1", current_panel)
      })
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Observers for active map selector (one for each panel)
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      observeEvent(input[[paste0("active_map_selector_", panel_type)]], {
        selected_map <- input[[paste0("active_map_selector_", panel_type)]]
        
        # Update THIS panel's active map
        active_map_per_panel[[panel_type]] <- selected_map
        
        # Update global active panel
        active_panel(panel_type)
        
        cat("Panel:", panel_type, "- Active map changed to:", selected_map, "\n")
        
        invalidateLater(100, session)
        isolate({
          restoreMapParameters(selected_map, panel_type)
        })
      }, ignoreInit = TRUE)
    })
    
    # Observers for temporality changes - ONLY UPDATE STATE
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      observeEvent(input[[paste0("filter_options_", panel_type)]], {
        current_map <- active_map_per_panel[[panel_type]]
        req(current_map)
        temporalite <- input[[paste0("filter_options_", panel_type)]]
        
        # Update state only
        map_params[[current_map]][[panel_type]]$temporalite <- temporalite
        
        session$sendCustomMessage(
          type = "updateDatePickerTemporalite",
          message = list(
            id = session$ns(paste0("custom_date_", panel_type)),
            temporalite = temporalite
          )
        )
        
        cat("Map:", current_map, "Panel:", panel_type, "- Temporality:", temporalite, "\n")
      }, ignoreInit = TRUE)
    })
    
    # Observers for date changes - ONLY UPDATE STATE
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      observeEvent(input[[paste0("custom_date_", panel_type)]], {
        current_map <- active_map_per_panel[[panel_type]]
        req(current_map)
        date_value <- input[[paste0("custom_date_", panel_type)]]
        
        # Update state only
        map_params[[current_map]][[panel_type]]$date <- date_value
        cat("Map:", current_map, "Panel:", panel_type, "- Date:", date_value, "\n")
      }, ignoreInit = TRUE)
    })
    
    # Observers for indice changes - ONLY UPDATE STATE
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      observeEvent(input[[paste0("filter_", panel_type, "_options")]], {
        current_map <- active_map_per_panel[[panel_type]]
        req(current_map)
        indice_value <- input[[paste0("filter_", panel_type, "_options")]]
        
        # Update state only
        map_params[[current_map]][[panel_type]]$indice <- indice_value
        map_params[[current_map]][[panel_type]]$panel_type <- panel_type
        
        cat("Map:", current_map, "Panel:", panel_type, "- Indice:", indice_value, "\n")
      }, ignoreInit = TRUE)
    })
    
    # NEW: Update button observers - TRIGGER RENDERING
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      observeEvent(input[[paste0("update_map_", panel_type)]], {
        current_map <- active_map_per_panel[[panel_type]]
        req(current_map)
        
        # Increment update trigger to signal rendering
        map_params[[current_map]][[panel_type]]$update_trigger <- 
          map_params[[current_map]][[panel_type]]$update_trigger + 1
        
        cat("UPDATE BUTTON CLICKED - Map:", current_map, "Panel:", panel_type, 
            "Trigger:", map_params[[current_map]][[panel_type]]$update_trigger, "\n")
      }, ignoreInit = TRUE)
    })
    
    # Return values
    return(list(
      map_params = map_params,                     
      active_map_per_panel = active_map_per_panel, 
      active_panel = active_panel                   
    ))
  })
}