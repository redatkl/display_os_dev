# geomonitoring module
source("R/modules/geo/sidebar.R")
source("R/modules/geo/map_layout_module.R")


geo_ui <- function(id) {
  ns <- NS(id)

    
tagList(
  sidebarModuleUI(ns("sidebar1")),
  
  # Map layout area
  mapLayoutModuleUI(ns("map_layout"))
  
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

      zone <- if (sidebar_vals$zone_andzoa_on()) zone_andzoa else NULL

      leaflet_id <- paste0("map_layout-", map_id, "-map")
      cat("Rendering", map_id, "(", leaflet_id, ") with:", indice, temporalite, date,
          "| zone ANDZOA:", !is.null(zone), "\n")

      rast <- fetch_raster(indice, temporalite, date, conn)
      add_raster_layer(leaflet_id, rast, indice, zone = zone)
    }

    observeEvent(sidebar_vals$render_request(), {
      req  <- sidebar_vals$render_request()
      req(!is.null(req))
      last_requests[[req$map_id]] <- req
      render_map(req)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Re-render every map that already has a raster when the zone switch flips
    observeEvent(sidebar_vals$zone_andzoa_on(), {
      for (map_id in names(reactiveValuesToList(last_requests))) {
        render_map(last_requests[[map_id]])
      }
    }, ignoreInit = TRUE)
  })
}


