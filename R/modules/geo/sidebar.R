# the sidebar module 
source("R/functions/toggle_button.R")
source("R/functions/datepicker.R")
source("R/functions/panel_component.R")
source("R/functions/legend.R")
source("R/functions/db_config.R")

sidebarModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include custom CSS and JS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/toggle_button.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/datepicker.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/legend.css"),
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
    
    # ── Default date: current month/year in French format ──────────────────
    # This MUST match what customDatePickerInput() renders on page load,
    # otherwise the datepicker shows one date while map_params holds another.
    default_date <- current_date_fr()   # e.g. "mars 2026"
    
    default_panel_params <- function(indice) {
      list(temporalite = "mensuel", date = default_date, indice = indice, update_trigger = 0)
    }
    
    # Reactive values to store parameters for each map
    map_params <- reactiveValues(
      map1 = list(
        climate    = default_panel_params("precip"),
        vegetation = default_panel_params("NDVI"),
        water      = default_panel_params("NDWI"),
        soil       = default_panel_params("SM"),
        combined   = default_panel_params("VCI")
      ),
      map2 = list(
        climate    = default_panel_params("precip"),
        vegetation = default_panel_params("NDVI"),
        water      = default_panel_params("NDWI"),
        soil       = default_panel_params("SM"),
        combined   = default_panel_params("VCI")
      ),
      map3 = list(
        climate    = default_panel_params("precip"),
        vegetation = default_panel_params("NDVI"),
        water      = default_panel_params("NDWI"),
        soil       = default_panel_params("SM"),
        combined   = default_panel_params("VCI")
      ),
      map4 = list(
        climate    = default_panel_params("precip"),
        vegetation = default_panel_params("NDVI"),
        water      = default_panel_params("NDWI"),
        soil       = default_panel_params("SM"),
        combined   = default_panel_params("VCI")
      )
    )
    
    # ── render_request: the ONLY signal geo_module should observe ─────────────
    # Set atomically when user clicks "Mettre à jour". Contains a snapshot of
    # exactly what should be rendered, so geo_module never needs active_panel().
    render_request <- reactiveVal(NULL)
    
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
    
    # Map panel IDs to their types
    panel_types <- list(
      panel1 = "climate",
      panel2 = "vegetation", 
      panel3 = "water",
      panel4 = "soil",
      panel5 = "combined"
    )
    
    # ── Panel activation ───────────────────────────────────────────────────────
    observeEvent(input$active_panel_type, {
      req(input$active_panel_type)
      new_panel   <- input$active_panel_type
      active_panel(new_panel)
      current_map <- active_map_per_panel[[new_panel]]
      cat("Panel switched to:", new_panel, "- Active map:", current_map, "\n")
      invalidateLater(100, session)
      isolate(restoreMapParameters(current_map, new_panel))
    }, ignoreInit = TRUE)
    
    # ── Map selector UI ────────────────────────────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      output[[paste0("map_selector_ui_", panel_type)]] <- renderUI({
        selectizeInput(
          session$ns(paste0("active_map_selector_", panel_type)),
          label = NULL, choices = c("Carte 1" = "map1"), selected = "map1",
          width = "100%", options = list(placeholder = "S\u00e9lectionner une carte"))
      })
      outputOptions(output, paste0("map_selector_ui_", panel_type), suspendWhenHidden = FALSE)
      
      observeEvent(input$map_layout_selected, {
        req(input$map_layout_selected)
        map_choices <- switch(input$map_layout_selected,
                              "layout1" = c("Carte 1" = "map1"),
                              "layout2" = c("Carte 1" = "map1", "Carte 2" = "map2"),
                              "layout4" = c("Carte 1" = "map1", "Carte 2" = "map2",
                                            "Carte 3" = "map3", "Carte 4" = "map4"),
                              c("Carte 1" = "map1"))
        
        cur <- active_map_per_panel[[panel_type]]
        if (!cur %in% names(map_choices)) {
          active_map_per_panel[[panel_type]] <- "map1"
          cur <- "map1"
        }
        updateSelectizeInput(session, paste0("active_map_selector_", panel_type),
                             choices = map_choices, selected = cur)
      }, ignoreInit = FALSE)
    })
    
    # ── Restore parameters when switching panel/map ────────────────────────────
    restoreMapParameters <- function(map_id, panel_type) {
      p <- map_params[[map_id]][[panel_type]]
      cat("Restoring parameters for", map_id, "in panel", panel_type, "\n")
      cat("  Temporalite:", p$temporalite, "  Date:", p$date, "  Indice:", p$indice, "\n")
      
      session$sendCustomMessage("updateToggleSwitch", list(
        id = session$ns(paste0("filter_options_", panel_type)), value = p$temporalite))
      session$sendCustomMessage("updateDatePickerValue", list(
        id = session$ns(paste0("custom_date_", panel_type)),
        date = p$date, temporalite = p$temporalite))
      if (!is.null(p$indice))
        session$sendCustomMessage("updateToggleSwitch", list(
          id = session$ns(paste0("filter_", panel_type, "_options")), value = p$indice))
    }
    
    # ── Layout change ──────────────────────────────────────────────────────────
    observeEvent(input$map_layout_selected, {
      cat("Map layout changed to:", input$map_layout_selected, "\n")
      cur <- active_panel()
      active_map_per_panel[[cur]] <- "map1"
      invalidateLater(100, session)
      isolate({ restoreMapParameters("map1", active_panel()) })
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # ── Active map selector per panel ─────────────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      observeEvent(input[[paste0("active_map_selector_", panel_type)]], {
        sel <- input[[paste0("active_map_selector_", panel_type)]]
        active_map_per_panel[[panel_type]] <- sel
        active_panel(panel_type)
        cat("Panel:", panel_type, "- Active map changed to:", sel, "\n")
        invalidateLater(100, session)
        isolate(restoreMapParameters(sel, panel_type))
      }, ignoreInit = TRUE)
    })
    
    # ── Temporality change: update state only ─────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      observeEvent(input[[paste0("filter_options_", panel_type)]], {
        cur <- active_map_per_panel[[panel_type]]
        req(cur)
        tmp <- input[[paste0("filter_options_", panel_type)]]
        isolate({ map_params[[cur]][[panel_type]]$temporalite <- tmp })
        session$sendCustomMessage("updateDatePickerTemporalite", list(
          id = session$ns(paste0("custom_date_", panel_type)), temporalite = tmp))
        cat("Map:", cur, "Panel:", panel_type, "- Temporality:", tmp, "\n")
      }, ignoreInit = TRUE)
    })
    
    # ── Date change: update state only ────────────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      observeEvent(input[[paste0("custom_date_", panel_type)]], {
        cur <- active_map_per_panel[[panel_type]]
        req(cur)
        dv <- input[[paste0("custom_date_", panel_type)]]
        # Guard: skip raw numeric values from unrelated Shiny inputs
        if (!is.null(dv) && !is.numeric(dv) && nchar(as.character(dv)) > 4) {
          isolate({ map_params[[cur]][[panel_type]]$date <- dv })
          cat("Map:", cur, "Panel:", panel_type, "- Date:", dv, "\n")
        }
      }, ignoreInit = FALSE)
    })
    
    # ── Indice change: update state only ──────────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      observeEvent(input[[paste0("filter_", panel_type, "_options")]], {
        cur <- active_map_per_panel[[panel_type]]
        req(cur)
        iv <- input[[paste0("filter_", panel_type, "_options")]]
        isolate({ map_params[[cur]][[panel_type]]$indice <- iv })
        cat("Map:", cur, "Panel:", panel_type, "- Indice:", iv, "\n")
      }, ignoreInit = TRUE)
    })
    
    # ── UPDATE BUTTON: snapshot params → render_request ───────────────────────
    # This is the ONLY place that triggers a map render.
    # We snapshot all needed params here so geo_module never needs active_panel().
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      observeEvent(input[[paste0("update_map_", panel_type)]], {
        cur <- active_map_per_panel[[panel_type]]
        req(cur)
        p <- isolate(map_params[[cur]][[panel_type]])
        
        cat("UPDATE BUTTON CLICKED - Map:", cur, "Panel:", panel_type, "\n")
        cat("  Rendering:", p$indice, "/", p$temporalite, "/", p$date, "\n")
        
        # Atomic snapshot — geo_module observes this single reactive
        render_request(list(
          map_id     = cur,
          panel_type = panel_type,
          indice     = p$indice,
          temporalite = p$temporalite,
          date       = p$date,
          nonce      = as.numeric(Sys.time())  # ensures reactivity even for identical params
        ))
      }, ignoreInit = TRUE)
    })
    
    # ── Active map highlight ───────────────────────────────────────────────────
    observe({
      cur <- active_panel()
      req(cur)
      am <- active_map_per_panel[[cur]]
      req(am)
      session$sendCustomMessage("highlightActiveMap", list(mapId = am))
    }) %>% bindEvent(
      active_panel(),
      lapply(names(panel_types), function(p) {
        input[[paste0("active_map_selector_", panel_types[[p]])]]
      }),
      ignoreInit = FALSE
    )
    
    # ── Dynamic legends ────────────────────────────────────────────────────────
    lapply(names(panel_types), function(panel) {
      panel_type <- panel_types[[panel]]
      
      output[[paste0("legend_ui_", panel_type)]] <- renderUI({
        cur    <- active_map_per_panel[[panel_type]]
        req(cur)
        indice <- map_params[[cur]][[panel_type]]$indice
        if (is.null(indice) || indice == "") return(create_placeholder_legend())
        create_dynamic_legend(indice, get_color_config(indice))
      })
      outputOptions(output, paste0("legend_ui_", panel_type), suspendWhenHidden = FALSE)
      
      observeEvent(input[[paste0("filter_", panel_type, "_options")]], {
        output[[paste0("legend_ui_", panel_type)]] <- renderUI({
          iv <- input[[paste0("filter_", panel_type, "_options")]]
          create_dynamic_legend(iv, get_color_config(iv))
        })
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("active_map_selector_", panel_type)]], {
        invalidateLater(50, session)
        isolate({
          sel    <- input[[paste0("active_map_selector_", panel_type)]]
          indice <- map_params[[sel]][[panel_type]]$indice
          output[[paste0("legend_ui_", panel_type)]] <- renderUI({
            create_dynamic_legend(indice, get_color_config(indice))
          })
        })
      }, ignoreInit = TRUE)
    })
    
    # ── Return ─────────────────────────────────────────────────────────────────
    return(list(
      map_params           = map_params,
      active_map_per_panel = active_map_per_panel,
      active_panel         = active_panel,
      render_request       = render_request 
    ))
  })
}