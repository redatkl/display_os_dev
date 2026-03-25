source("R/functions/db_config.R")

# Cartes module UI
cartes_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/analyse_temporelle.css")
    ),
    
    div(
      class = "control-bar",
      
      # Indice
      div(class = "control-group",
          tags$span("Indice", class = "control-label"),
          selectInput(ns("indice"), label = NULL,
                      choices = c(
                        "SPI"    = "SPI",
                        "LST"    = "LST",
                        "ANDVI"  = "ANDVI",
                        "SM"     = "SM",
                        "CDI"    = "CDI"
                      ),
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
                          selected = na.omit(unique(regions$nom_fr))[1],
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
      
      # temporalite dv
      div(class = "control-group",
          tags$span("Temporalité", class = "control-label"),
          selectInput(ns("temporalite"), label = NULL,
                      choices = c(
                        "Mensuel"     = "mensuel",
                        "Trimestriel" = "trimestriel",
                        "Annuel"      = "annuel"
                      ),
                      selected = "mensuel",
                      width = "120px"
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
      style = "margin-top: 20px; position: relative;",
      shinycssloaders::withSpinner(
      uiOutput(ns("figure_display")),
      type    = 4,        # spinner style 1-8
      color   = "#4a7c59", #  green color
      size    = 0.5,
      caption = tags$img(
        src = "logos/logo.png", height = "30px", style = "
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        height: 30px;
        margin-top: 0;
      "
      )
      )
    )
  )
  
}

cartes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    conn <- init_db()
    
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
    
    # ── Helper: resolve the right sf object + name column for the niveau ──────
    get_polygons_sf <- function(niveau) {
      switch(niveau,
             "National" = list(
               sf       = maroc,
               name_col = "nom_fr"
             ),
             "Régional" = list(
               sf       = regions,
               name_col = "nom_fr"
             ),
             "Provincial" = {
               code <- paste0(sprintf("%02d",
                                      regions$id_region[regions$nom_fr == input$region_filter]), ".")
               list(
                 sf       = provinces[provinces$Code_Region == code, ],
                 name_col = "Nom_Provinces"
               )
             },
             "Communal" = {
               list(
                 sf       = communes[communes$Nom_Provinces == input$province_commune_filter, ],
                 name_col = "commune"
               )
             },
             NULL
      )
    }
    
    # Prepare data for the animated gif fetch data first
    data_reactive <- reactive({
      req(input$search > 0)  # only after first click
      req(input$indice, input$niveau, input$temporalite)
     
      isolate({
        
        
        # 1. Get all available dates from 2015 ──────────────────────────────
        dates <- get_available_dates(input$indice, input$temporalite, conn, from_year = 2024)
        
        if (length(dates) == 0) return(NULL) 
        
        
        # 2. Resolve polygons ────────────────────────────────────────────────
        poly_info <- get_polygons_sf(input$niveau)
        
        if (is.null(poly_info)) return(NULL)
        
        polys    <- sf::st_as_sf(poly_info$sf)         # ensure sf class
        name_col <- poly_info$name_col
        
        # 3. Loop over dates: fetch raster → zonal mean ──────────────────────
        all_frames <- lapply(seq_along(dates), function(i) {
          date_str <- dates[i]
          
          rast <- tryCatch(
            fetch_raster(input$indice, input$temporalite, date_str, conn),
            error = function(e) NULL
          )
          if (is.null(rast)) return(NULL)
          
          # Clean NoData values
          rast <- calc(rast, fun = function(x) {
            x[x < -9999 | is.infinite(x)] <- NA
            x
          })
          
          # Zonal mean per polygon (exactextractr is faster & more accurate than raster::extract)
          means <- tryCatch(
            exactextractr::exact_extract(rast, polys, fun = "mean"),
            error = function(e) {
              # fallback to raster::extract if exactextractr unavailable
              tryCatch(
                sapply(1:nrow(polys), function(j) {
                  v <- raster::extract(rast, as(polys[j, ], "Spatial"))[[1]]
                  if (is.null(v)) NA_real_ else mean(v, na.rm = TRUE)
                }),
                error = function(e2) rep(NA_real_, nrow(polys))
              )
            }
          )
          
          data.frame(
            name       = polys[[name_col]],
            mean_value = means,
            date_label = date_str,
            frame_idx  = i,          # used to order animation frames
            stringsAsFactors = FALSE
          )
        })
        
        # 4. Combine & validate ──────────────────────────────────────────────
        df <- do.call(rbind, Filter(Negate(is.null), all_frames))
        
        if (is.null(df) || nrow(df) == 0) return(NULL)
        
        cat(sprintf("[cartes] Fetched %d frames × %d units for %s (%s)\n",
                    length(unique(df$frame_idx)),
                    length(unique(df$name)),
                    input$indice, input$temporalite))
        
        list(
          df       = df,
          polys    = polys,
          name_col = name_col,
          config   = get_color_config(input$indice),
          indice   = input$indice,
          niveau   = input$niveau,
          temp     = input$temporalite
        )
      })
    })
        
    # ── figure_display will go here (animation rendering) ─────────────────────
    output$figure_display <- renderUI({
      req(data_reactive())
      # → next step: build gganimate / plotly animated choropleth here
      tags$p("Données chargées — rendu de l'animation en cours…")
    })
 
    # ── Render figure on search click ──────────────────────────────────────────
    # output$figure_display <- renderUI({
    #   req(input$search > 0)  # only after first click
    #   
    #   isolate({
    # 
    #     
    #   })
    # })
    # 
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