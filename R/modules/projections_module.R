# Page: Projections

projections_ui <- function(id) {
  ns <- NS(id)
  
  # Region choices: value = slug, label = human name from JSON
  region_choices <- c(
    setNames(
      names(projections_mapping$regions),
      sapply(projections_mapping$regions, function(r) r$label)
    )
  )
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/projections_module.css")
    ),
    
    # ── Decorative sidebar (icons only) ───────────────────────────────────────
    div(
      class = "custom-sidebar projections-sidebar",
      div(
        class = "sidebar-icons",
        div(
          class = "sidebar-reporting-icon",
          tags$img(src = "icons_svg/chart-area-solid-full.svg",
                   width = "22px", height = "22px",
                   style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Projections climatiques")
        ),
        div(
          class = "sidebar-reporting-icon",
          tags$img(src = "icons_svg/morocco.svg",
                   width = "22px", height = "22px",
                   style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Carte des étages")
        ),
        div(class = "sidebar-reporting-divider"),
        div(
          class = "sidebar-reporting-icon",
          tags$img(src = "icons_svg/report.png",
                   width = "22px", height = "22px",
                   style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Rapports")
        )
      )
    ),
    
    # ── Main content ──────────────────────────────────────────────────────────
    div(
      class = "projections-main-content",
      div(
        class = "projections-main-wrapper",
        
        # ── Control bar ───────────────────────────────────────────────────────
        div(
          class = "control-bar",
          
          # Temporalité
          div(
            class = "control-group",
            tags$span("Temporalité", class = "control-label"),
            selectInput(ns("temporalite"), label = NULL,
                        choices  = c("Annuel"     = "yearly",
                                     "Mensuel"    = "monthly",
                                     "Saisonnier" = "seasonal"),
                        selected = "yearly",
                        width    = "130px")
          ),
          
          # Région
          div(
            class = "control-group",
            tags$span("Région", class = "control-label"),
            selectInput(ns("region"), label = NULL,
                        choices  = region_choices,
                        selected = "all",
                        width    = "230px")
          ),
          
          # Étage bioclimatique — updated server-side when region changes
          div(
            class = "control-group",
            tags$span("Étage bioclimatique", class = "control-label"),
            selectInput(ns("etage"), label = NULL,
                        choices  = "",
                        selected = "",
                        width    = "240px")
          ),
          
          # Search
          actionButton(ns("search"), label = NULL,
                       icon  = icon("magnifying-glass"),
                       class = "btn search-btn")
        ),
        
        # ── Figure + info ─────────────────────────────────────────────────────
        div(
          class = "projections-figure-area",
          div(class = "projections-png-area",   uiOutput(ns("figure_display"))),
          div(class = "projections-info-panel", uiOutput(ns("info_display")))
        )
      )
    )
  )
}


projections_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ── Helper: get étages list for a given region slug ───────────────────────
    # JSON structure: regions[[slug]]$etages is a data.frame with cols code/label
    get_etages_for_region <- function(region_slug) {
      if (region_slug == "all") {
        # Union of all étages across all regions, deduplicated
        all_df <- do.call(rbind, lapply(projections_mapping$regions, function(r) {
          as.data.frame(r$etages, stringsAsFactors = FALSE)
        }))
        all_df <- all_df[!duplicated(all_df$code), ]
        all_df[order(all_df$code), ]
      } else {
        as.data.frame(
          projections_mapping$regions[[region_slug]]$etages,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # ── Update étage dropdown when region changes ─────────────────────────────
    observeEvent(input$region, {
      etages_df <- get_etages_for_region(input$region %||% "all")
      
      if (is.null(etages_df) || nrow(etages_df) == 0) {
        choices <- c("Aucun étage disponible" = "none")
      } else {
        choices <- c(
          setNames(etages_df$code,
                   paste0(etages_df$code, " — ", etages_df$label))
        )
      }
      
      updateSelectInput(session, "etage", choices = choices, selected = "all")
    }, ignoreInit = FALSE)
    
    # ── File path resolver ────────────────────────────────────────────────────
    # yearly:   <folder>/yearly/<region>_<etage>.png
    # monthly:  <folder>/monthly/<region>_<etage>_monthly.png
    # seasonal: <folder>/seasonal/<region>_<etage>_seasonal.png
    get_figure_path <- function(temporalite, region_slug, etage_code) {
      suffix <- switch(temporalite,
                       yearly   = ".png",
                       monthly  = "_monthly.png",
                       seasonal = "_seasonal.png")
      
      base <- if (region_slug == "all" && etage_code == "all") {
        "all_all"
      } else if (region_slug == "all") {
        paste0("all_", etage_code)
      } else if (etage_code == "all") {
        paste0(region_slug, "_all")
      } else {
        paste0(region_slug, "_", etage_code)
      }
      
      filename  <- paste0(base, suffix)
      real_path <- file.path(figures_proj_path, temporalite, filename)
      web_path  <- paste0("figures_proj/", temporalite, "/", filename)
      list(real = real_path, web = web_path, filename = filename)
    }
    
    # ── Label helpers ─────────────────────────────────────────────────────────
    get_region_label <- function(slug) {
      if (slug == "all") return("Toutes les régions")
      projections_mapping$regions[[slug]]$label %||% slug
    }
    
    get_etage_label <- function(code) {
      if (code == "all")  return("Tous les étages")
      if (code == "none") return("—")
      # Search across all regions for matching code label
      for (r in projections_mapping$regions) {
        etages_df <- as.data.frame(r$etages, stringsAsFactors = FALSE)
        hit <- etages_df$label[etages_df$code == code]
        if (length(hit) > 0) return(paste0(code, " — ", hit[1]))
      }
      code
    }
    
    get_temp_label <- function(t) {
      c(yearly = "Annuel", monthly = "Mensuel", seasonal = "Saisonnier")[t]
    }
    
    # ── Figure output ─────────────────────────────────────────────────────────
    output$figure_display <- renderUI({
      req(input$search > 0)
      isolate({
        
        if (isTruthy(input$etage) && input$etage == "none") {
          return(div(
            class = "proj-message proj-warning",
            icon("triangle-exclamation"),
            tags$p("Aucun étage bioclimatique disponible pour cette région.")
          ))
        }
        
        paths <- get_figure_path(input$temporalite, input$region, input$etage)
        
        if (!file.exists(paths$real)) {
          div(
            class = "proj-message proj-error",
            icon("circle-exclamation"),
            tags$p(paste("Figure non disponible :", paths$filename))
          )
        } else {
          div(
            style = "position:relative; width:100%; height:100%; display:flex; flex-direction:column;",
            
            # ── Zoom controls ──────────────────────────────────────────────
            div(
              class = "proj-zoom-bar",
              tags$button(
                class   = "proj-zoom-btn",
                onclick = "projZoom('proj-figure-img', 10)",
                title   = "Zoom +",
                icon("magnifying-glass-plus")
              ),
              tags$button(
                class   = "proj-zoom-btn",
                onclick = "projZoom('proj-figure-img', -10)",
                title   = "Zoom -",
                icon("magnifying-glass-minus")
              ),
              tags$button(
                class   = "proj-zoom-btn",
                onclick = "projZoomReset('proj-figure-img')",
                title   = "Réinitialiser",
                icon("arrows-rotate")
              )
            ),
            
            # ── Scrollable image container ─────────────────────────────────
            div(
              class = "proj-img-scroll",
              tags$img(
                id    = "proj-figure-img",
                src   = paths$web,
                style = "width:60%; height:auto; display:block; margin:0 auto; transition:width 0.2s ease;",
                alt   = paste("Projection", input$temporalite, input$region, input$etage)
              )
            ),
            
            # ── Zoom JS ────────────────────────────────────────────────────
            tags$script(HTML("
              function projZoom(imgId, delta) {
                var img = document.getElementById(imgId);
                if (!img) return;
                var current = parseFloat(img.style.width) || 60;
                var next = Math.min(Math.max(current + delta, 20), 200);
                img.style.width = next + '%';
              }
              function projZoomReset(imgId) {
                var img = document.getElementById(imgId);
                if (img) img.style.width = '60%';
              }
            "))
          )
        }
      })
    })
    
    # ── Info panel ────────────────────────────────────────────────────────────
    output$info_display <- renderUI({
      req(input$search > 0)
      isolate({
        
        # Count available étages for this region
        etages_df  <- get_etages_for_region(input$region %||% "all")
        n_etages   <- if (is.null(etages_df)) 0L else nrow(etages_df)
        
        div(
          class = "projections-info-card",
          
          div(class = "info-title", icon("circle-info"), " Sélection"),
          
          div(class = "info-row",
              tags$span(class = "info-label", "Temporalité"),
              tags$span(class = "info-value", get_temp_label(input$temporalite))),
          
          div(class = "info-row",
              tags$span(class = "info-label", "Région"),
              tags$span(class = "info-value", get_region_label(input$region))),
          
          div(class = "info-row",
              tags$span(class = "info-label", "Étage bioclimatique"),
              tags$span(class = "info-value", get_etage_label(input$etage))),
          
          div(class = "info-row",
              tags$span(class = "info-label", "Étages Bioclimatiques dans cette région"),
              tags$span(class = "info-value info-badge", paste0(n_etages, " étage(s)"))),
          
          tags$hr(style = "margin:12px 0; border-color:#e0e0e0;"),
          
          # ── Static Scenario Info Section ───────────────────────────────────────
          div(
            style = "font-size:12.5px; color:#444; line-height:1.5;",
            
            div(style = "margin-bottom: 8px;",
                tags$strong("SSP2-4.5 : ", style = "color:#2c3e50;"),
                "Correspond à un futur modéré où les efforts pour réduire la pollution permettent de limiter les effets de changement climatique."
            ),
            
            div(
              tags$strong("SSP5-8.5 : ", style = "color:#2c3e50;"),
              "Le scénario du pire des cas, où la consommation massive d'énergies fossiles continue sans freins."
            )
          )
        )
      })
    })
    
  })
}