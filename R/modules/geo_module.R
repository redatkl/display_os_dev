# geomonitoring module
source("R/modules/geo/sidebar.R")
source("R/modules/geo/map_layout_module.R")


geo_ui <- function(id) {
  ns <- NS(id)

    
tagList(
  sidebarModuleUI(ns("sidebar1")),
  
  # Map layout area
  mapLayoutModuleUI(ns("map_layout")),

  # Single banner for the whole map area (shown when the ANDZOA zone is on)
  uiOutput(ns("zone_banner"))

  )
}

geo_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create connection
    conn <- init_db()
    #onStop(function() dbDisconnect(conn))
    
    sidebar_vals <- sidebarModuleServer("sidebar1")
    
    # Create a reactive for map layout that defaults to "layout1"
    selected_layout <- reactive({
      layout <- input$`sidebar1-map_layout_selected`
      cat("Selected layout from sidebar:", layout, "\n")  # Debug
      if (is.null(layout) || layout == "") {
        "layout1"  # Default
      } else {
        layout
      }
    })
    
    # Initialize map layout with reactive layout from sidebar
    map_modules <- mapLayoutModuleServer(
      "map_layout",
      layout = selected_layout
    )
    
    # ── SINGLE render observer ─────────────────────────────────────────────────
    # sidebar_vals$render_request() is set atomically when the user clicks
    # "Mettre à jour". It already contains the exact map_id + params to render,
    # so we never need active_panel() here — eliminating the timing race.
    # ── ANDZOA action zone (loaded once, dissolved to a single polygon) ────────
    zone_andzoa <- tryCatch(
      sf::st_read("data/zone_d_action/zone_action_ANDZOA.geojson", quiet = TRUE) %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_as_sf(),
      error = function(e) { warning("ANDZOA zone not loaded: ", e$message); NULL }
    )

    # Cities of the zone (already filtered to the zone), shown as indication
    villes_andzoa <- tryCatch(
      sf::st_read("data/zone_d_action/villes_zone_andzoa.geojson", quiet = TRUE),
      error = function(e) { warning("ANDZOA cities not loaded: ", e$message); NULL }
    )

    output$zone_banner <- renderUI({
      if (isTRUE(sidebar_vals$zone_andzoa_on())) {
        div(class = "zone-banner", "ZONE D'ACTION ANDZOA")
      }
    })

    # Last render request per map, so the zone switch can re-render without
    # the user having to click "Mettre à jour" again.
    last_requests <- reactiveValues()

    render_map <- function(req) {
      map_id      <- req$map_id        # "map1" / "map2" / "map3" / "map4"
      indice      <- req$indice
      temporalite <- req$temporalite
      date        <- req$date

      # Layout compatibility guard
      layout <- selected_layout()
      map_visible <- switch(layout,
                            "layout1" = map_id == "map1",
                            "layout2" = map_id %in% c("map1", "map2"),
                            "layout4" = TRUE,
                            map_id == "map1"
      )
      if (!map_visible) {
        cat("Skipping render — map", map_id, "not visible in layout", layout, "\n")
        return()
      }

      zone   <- if (sidebar_vals$zone_andzoa_on()) zone_andzoa else NULL
      cities <- if (!is.null(zone)) villes_andzoa else NULL

      leaflet_id <- paste0("map_layout-", map_id, "-map")
      cat("Rendering", map_id, "(", leaflet_id, ") with:", indice, temporalite, date,
          "| zone ANDZOA:", !is.null(zone), "\n")

      rast <- fetch_raster(indice, temporalite, date, conn)
      add_raster_layer(leaflet_id, rast, indice, zone = zone, cities = cities)
    }

    observeEvent(sidebar_vals$render_request(), {
      req  <- sidebar_vals$render_request()
      req(!is.null(req))
      last_requests[[req$map_id]] <- req
      render_map(req)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Re-render every map that already has a raster when the zone switch flips;
    # maps without an indice just get the zone outline + zoom.
    observeEvent(sidebar_vals$zone_andzoa_on(), {
      zone <- if (isTRUE(sidebar_vals$zone_andzoa_on())) zone_andzoa else NULL
      rendered <- names(reactiveValuesToList(last_requests))

      layout <- selected_layout()
      visible_maps <- switch(layout,
                             "layout1" = "map1",
                             "layout2" = c("map1", "map2"),
                             "layout4" = paste0("map", 1:4),
                             "map1")

      for (map_id in visible_maps) {
        if (map_id %in% rendered) {
          render_map(last_requests[[map_id]])
        } else {
          show_zone_only(paste0("map_layout-", map_id, "-map"), zone,
                         if (!is.null(zone)) villes_andzoa else NULL)
        }
      }
    }, ignoreInit = TRUE)
  })
}


