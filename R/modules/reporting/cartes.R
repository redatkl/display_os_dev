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
                        "ALST"    = "LST_A",
                        "ANDVI"   = "ANDVI",
                        "SM_root"     = "SM_A_ROOT",
                        "SM_surface"  = "SM_A_SURFACE",
                        "CDI"    = "CDI"
                      ),
                      selected = "SPI",
                      width = "120px"
          )
      ),
      
      # Niveau
      div(class = "control-group",
          tags$span("Niveau", class = "control-label"),
          selectInput(ns("niveau"), label = NULL,
                      choices = c("Régional", "Provincial", "Communal"),
                      selected = "Régional",
                      width = "120px"
          ),
          # conditionalPanel(
          #   condition = "input.niveau == 'Régional'",
          #   ns = ns,
          #   selectInput(ns("region_detail"), label = NULL,
          #               choices = regions$nom_fr,
          #               selected = NULL,
          #               width = "210px"
          #   )
          # ),
          
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
              )
              # # Second: provinces filtered by region (updated server-side)
              # selectInput(ns("province_detail"), label = NULL,
              #             choices = NULL,  
              #             width = "210px"
              # )
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
              )
              # selectInput(ns("commune_detail"), label = NULL,
              #             choices = NULL,
              #             selected = NULL,
              #             width = "210px"
              # )
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
      style = "display:flex; flex-direction:row; align-items:stretch; width: 100%; margin-top:20px; min-height: auto;",
    div(
      class = "gif-area",
      style = "flex:0 0 50%; display:flex; justify-content:flex-end; align-items:center;",
      shinycssloaders::withSpinner(
        imageOutput(ns("anim_gif"), height = "auto", width = "auto"),
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
    ),
    div(
      class = "legend-table-area",
      style = "flex: 0 0 50%; display: flex; align-items: center; justify-content: flex-end;",
      uiOutput(ns("legend_table"))
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
      #req(input$indice, input$niveau, input$temporalite)
     
      isolate({
        
        
        # 1. Get all available dates from 2015 ──────────────────────────────
        dates <- get_available_dates(input$indice, input$temporalite, conn, from_year = 2024)
        
        if (length(dates) == 0) return(NULL) 
        
        
        # 2. Resolve polygons ────────────────────────────────────────────────
        poly_info <- get_polygons_sf(input$niveau)
        
        if (is.null(poly_info)) return(NULL)
        
        polys    <- sf::st_as_sf(poly_info$sf)         # ensure sf class
        polys <- sf::st_make_valid(polys)
        polys <- polys[!sf::st_is_empty(polys), ]
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
            frame_idx  = i,     
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
 
    # Build animation
    output$anim_gif <- renderImage({
      data <- data_reactive()
      req(data)
      
      df       <- data$df
      polys    <- data$polys
      name_col <- data$name_col
      config   <- data$config
      
      # ── 1. Build df_sf: one row per polygon per date ──────────────────────────
      polys$name <- polys[[name_col]]
      geom_col   <- attr(polys, "sf_column")
      polys_plain <- sf::st_drop_geometry(polys)[, "name", drop = FALSE]
      
      dates_ordered <- unique(df$date_label[order(df$frame_idx)])
      
      df_sf <- do.call(rbind, lapply(dates_ordered, function(d) {
        sub               <- df[df$date_label == d, ]
        result            <- polys
        result$mean_value <- sub$mean_value[match(polys$name, sub$name)]
        result$date_label <- d
        result$frame_idx  <- sub$frame_idx[match(polys$name, sub$name)][1]
        result
      }))
      
      # Keep date_label as ordered factor so gganimate respects chronological order
      df_sf$date_label <- factor(df_sf$date_label, levels = dates_ordered)
      
      # ── 2. Color scale ────────────────────────────────────────────────────────
      finite_breaks <- config$breaks[is.finite(config$breaks)]
      color_scale   <- scales::rescale(finite_breaks)
      
      # ── 3. Build ggplot ───────────────────────────────────────────────────────
      p <- ggplot2::ggplot(df_sf) +
        ggplot2::geom_sf(
          ggplot2::aes(fill = mean_value),
          color     = "white",
          linewidth = 0.3
        ) +
        ggplot2::scale_fill_gradientn(
          colors   = config$colors,
          values   = color_scale,
          limits   = range(finite_breaks),
          na.value = "grey80",
          name     = get_indice_title(data$indice)
        ) +
        ggplot2::labs(
          title    = paste(get_indice_title(data$indice), "—", data$niveau),
          subtitle = "Période : {closest_state}"         # gganimate fills this in
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position  = "none",
          plot.title       = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle    = ggplot2::element_text(hjust = 0.5, size = 12, color = "#047857")
        ) +
        # ── 4. gganimate transition ───────────────────────────────────────────
        gganimate::transition_states(
          date_label,
          transition_length = 1,   
          state_length      = 2   
        ) +
        gganimate::ease_aes("linear")
      
      # ── 5. Render to temp GIF file ────────────────────────────────────────────
      gif_path <- tempfile(fileext = ".gif")
      
      n_frames <- length(dates_ordered)
      
      gganimate::animate(
        p,
        nframes   = n_frames * 3,   # 3 rendering frames per state for smoothness
        fps       = 6,
        width     = 500,
        height    = 500,
        renderer  = gganimate::gifski_renderer(gif_path),
        res       = 96
      )
      
      
      # ── 6. Return for renderImage ─────────────────────────────────────────────
      list(
        src      = gif_path,
        alt      = "Carte animée",
        mimetype = "image/gif",
        width    = "80%",
        height   = "auto",
        style    = "display: block; margin: auto;"
      )
      
    }, deleteFile = TRUE) 
    
    # the legend as table
    output$legend_table <- renderUI({
      req(input$indice)
      
      # Title sentence
      title_text <- if (input$indice == "ANDVI") {
        "Catégorisation du degré de stress/santé de la végétation en fonction de l'ANDVI"
      } else {
        paste0("Catégorisation du degré de sécheresse/humidité en fonction du ", input$indice)
      }
      
      # SVG file
      svg_file <- if (input$indice == "ANDVI") {
        "georeporting_assets/tableau_legende_andvi.svg"
      } else {
        "georeporting_assets/tableau_legende.svg"
      }
      
      div(
        style = "display: flex; flex-direction: column; align-items: center; gap: 0; width: 100%;",
        tags$p(
          title_text,
          style = "
        font-size:14px; font-weight:700;
        text-align:center;
        font-family: T;
        margin:0 0 6px 0;
        color:#222;
        line-height:1.4;
      "
        ),
        tags$img(
          src   = svg_file,
          style = "width:80%; min-width: auto; height:auto; display:block;"
        )
      )
    })
    
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