# Analyse temporelle module UI
analyse_temporelle_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/analyse_temporelle.css"),
      tags$script(src = "js/map_selector.js"),
      tags$script(src = "https://d3js.org/d3.v7.min.js")
      ),
    
    div(
      class = "control-bar",
      
      # Indice
      div(class = "control-group",
          tags$span("Indice", class = "control-label"),
          selectInput(ns("indice"), label = NULL,
                      choices = "SPI",
                      selected = "SPI",
                      width = "60px"
          )
      ),
      
      # Niveau
      div(class = "control-group",
          tags$span("Niveau", class = "control-label"),
          selectInput(ns("niveau"), label = NULL,
                      choices = c("National", "Régional", "Provincial", "Communal"),
                      selected = "National",
                      width = "120px"
          ),
          conditionalPanel(
            condition = "input.niveau == 'Régional'",
            ns = ns,
            selectInput(ns("region_detail"), label = NULL,
                        choices = regions$nom_fr,
                        selected = NULL,
                        width = "210px"
            )
          ),
          
          conditionalPanel(
            condition = "input.niveau == 'Provincial'",
            ns = ns,
            div(
              style = "display: flex; align-items: center; gap: 6px;",
            # First: pick a region
            selectInput(ns("region_filter"), label = NULL,
                        choices = na.omit(unique(regions$nom_fr)),
                        selected = na.omit(unique(regions$nom_fr))[1],
                        width = "210px"
            ),
            # Second: provinces filtered by region (updated server-side)
            selectInput(ns("province_detail"), label = NULL,
                        choices = NULL,  
                        width = "210px"
            )
          )),
          
          conditionalPanel(
            condition = "input.niveau == 'Communal'",
            ns = ns,
            div(
              style = "display: flex; align-items: center; gap: 6px;",
              # First: pick a region
              selectInput(ns("region_commune_filter"), label = NULL,
                          choices = na.omit(unique(regions$nom_fr)),
                          selected = NULL,
                          width = "210px"
              ),
              # Second: provinces filtered by region (updated server-side)
              selectInput(ns("province_commune_filter"), label = NULL,
                          choices = NULL,  
                          width = "210px"
              ),
              selectInput(ns("commune_detail"), label = NULL,
                          choices = NULL,
                          selected = NULL,
                          width = "210px"
              )
            )
          )
          
      ),
      
      # Search button
      actionButton(ns("search"), label = NULL,
                   icon = icon("magnifying-glass"),
                   class = "btn search-btn"
      )    
      ),
      
      # Figure display area
      div(
        class = "figure-area",
        
        div(
          class = "png-figure-area",
          uiOutput(ns("figure_display"))
        ),
        
        div(
          class = "choice-map-area", 
          div(
            id = ns("map_container"),
            `data-ns`     = ns(""),          # passes "reporting-analyse-" to JS
            `data-niveau` = "National",
            style = "width: 100%; height: 100%; position: relative;",
            
            # Breadcrumb trail showing current drill-down path
            div(id = ns("map_breadcrumb"), class = "map-breadcrumb"),
            
            # The SVG D3 draws into
            tags$svg(
              id    = ns("map_svg"),
              style = "width: 100%; height: calc(100% - 30px); display: block;"
            ),
            
            # Reset button
            tags$button(
              id = ns("map_reset"),
              class = "map-reset-btn",
              onclick = paste0("resetMapSelector('", ns(""), "')"),
              icon("rotate-left"), " Reset"
            )
          )
          )

      )
        
      )

}

analyse_temporelle_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$search, {
      # Trigger analysis with selected filters
      message("Searching: ",
              "Indice=", input$indice,
              " Niveau=", input$niveau,
              " Région=", input$region_detail,
              " Province=", input$province_detail,
              " Commune=", input$commune_detail)
    })
    
    # When region changes, update provinces list
    update_provinces <- function(region_name) {
      req(region_name, nchar(region_name) > 0)
      code_region <- paste0(sprintf("%02d", regions$id_region[regions$nom_fr == region_name]), ".")
      filtered_provinces <- na.omit(provinces$Nom_Provinces[provinces$Code_Region == code_region])
      updateSelectInput(session, "province_detail", choices  = filtered_provinces, selected = filtered_provinces[1])
    }
    
    # Provinces for Communal panel
    update_communes_provinces <- function(region_name) {
      req(region_name, nchar(region_name) > 0)
      code_region <- paste0(sprintf("%02d", regions$id_region[regions$nom_fr == region_name]), ".")
      filtered_provinces <- na.omit(provinces$Nom_Provinces[provinces$Code_Region == code_region])
      updateSelectInput(session, "province_commune_filter",
                        choices = filtered_provinces, selected = filtered_provinces[1])
    }
    
    # Communes filtered by province
    update_communes <- function(province_name) {
      req(province_name, nchar(province_name) > 0)
      filtered_communes <- na.omit(
        commune_province_map$commune[commune_province_map$Nom_Provinces == province_name]
      )
      updateSelectInput(session, "commune_detail",
                        choices = filtered_communes, selected = filtered_communes[1])
    }
    
    # Provincial observers
    observeEvent(input$niveau, {
      if (input$niveau == "Provincial") {
        update_provinces(input$region_filter)
      }
    })
    observeEvent(input$region_filter, {
      if (input$niveau == "Provincial") {
        update_provinces(input$region_filter)
      }
    })
    
    # Communal observers - level 1: region → province
    observeEvent(input$niveau, {
      if (input$niveau == "Communal") update_communes_provinces(input$region_commune_filter)
    })
    observeEvent(input$region_commune_filter, {
      if (isTruthy(input$niveau) && input$niveau == "Communal") update_communes_provinces(input$region_commune_filter)
    })
    # Communal observers - level 2: province → commune
    observeEvent(input$province_commune_filter, {
      if (isTruthy(input$niveau) && input$niveau == "Communal") update_communes(input$province_commune_filter)
    })
    
    # Figures 
    # ── Figure lookup ──────────────────────────────────────────────────────────
    get_figure_path <- function(niveau, region_detail, province_detail, commune_detail) {
      
      build <- function(subfolder, json_df, name_val) {
        slug     <- json_df$slug[json_df$name == name_val]
        slug     <- slug[1]
        req(length(slug) > 0 && nchar(slug) > 0)
        real_path <- paste0(figures_path, "figures/", subfolder, "/", slug, "_spi12.png")
        web_path  <- paste0("figures/", subfolder, "/", slug, "_spi12.png")
        list(real = real_path, web = web_path)
      }
      
      switch(niveau,
             "National"   = list(real = paste0(figures_path, "figures/regions/national.png"),
                                 web  = "figures/regions/national.png"),
             "Régional"   = build("regions",   regions_filename,   region_detail),
             "Provincial" = build("provinces", provinces_filename, province_detail),
             "Communal"   = build("communes",  communes_filename,  commune_detail),
             NULL
      )
    }
    
    # ── Render figure on search click ──────────────────────────────────────────
    output$figure_display <- renderUI({
      req(input$search > 0)  # only after first click
      
      isolate({
        paths <- tryCatch(
          get_figure_path(input$niveau, input$region_detail, input$province_detail, input$commune_detail),
          error = function(e) NULL
        )
        
        if (is.null(paths) || !file.exists(paths$real)) {
          div(class = "figure-error",
              icon("circle-exclamation"),
              span(" Figure non disponible pour cette sélection.")
          )
        } else {
          # Shiny serves files from www/, so we will use addResourcePath
          tags$img(
            src   = paths$web,
            style = "max-width: 100%; height: auto; display: block; margin: 20px auto;",
            alt   = paste("Figure", input$niveau)
          )
        }
      })
    })
    
    # ──  Map Selection  ──────────────────────────────────────────
    observeEvent(input$map_selection, {
      sel <- input$map_selection
      req(!is.null(sel$name))   
      
      # 1. Sync the niveau dropdown
      updateSelectInput(session, "niveau", selected = sel$niveau)
      
      # 2. Sync the right detail dropdown
      if (sel$niveau == "Régional") {
        updateSelectInput(session, "region_detail", selected = sel$name)
        
      } else if (sel$niveau == "Provincial") {
        # Find which region this province belongs to
        region_name <- regions$nom_fr[
          regions$id_region == provinces$id_region[provinces$Nom_Provinces == sel$name][1]
        ]
        updateSelectInput(session, "region_filter", selected = region_name)
        shinyjs::delay(120, {
          updateSelectInput(session, "province_detail", selected = sel$name)
        })
        
      } else if (sel$niveau == "Communal") {
        province_name <- commune_province_map$Nom_Provinces[
          commune_province_map$commune == sel$name
        ][1]
        region_name <- regions$nom_fr[
          regions$id_region == provinces$id_region[provinces$Nom_Provinces == province_name][1]
        ]
        updateSelectInput(session, "region_commune_filter", selected = region_name)
        shinyjs::delay(120, {
          updateSelectInput(session, "province_commune_filter", selected = province_name)
        })
        shinyjs::delay(240, {
          updateSelectInput(session, "commune_detail", selected = sel$name)
        })
      }
    }, ignoreInit = TRUE)
    
    # Return reactive values for use by parent module
    return(reactive({
      list(
        indice      = input$indice,
        niveau      = input$niveau,
        trigger     = input$search
      )
    }))
  })
}