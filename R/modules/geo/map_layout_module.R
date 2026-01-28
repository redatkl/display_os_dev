# Map layout Module
source("R/modules/geo/map.R")

# Map layout module ui - ALL MAPS ALWAYS IN DOM
mapLayoutModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/map_layout.css"),
      tags$script(src = "js/map_sync.js"),
      tags$script(src = "js/map_layout.js")  # NEW
    ),
    
    # Static container - all 4 maps always present
    div(
      id = ns("map_container"),
      class = "map-layout layout-1",
      div(class = "map-container map-slot-1", mapModuleUI(ns("map1"))),
      div(class = "map-container map-slot-2", mapModuleUI(ns("map2"))),
      div(class = "map-container map-slot-3", mapModuleUI(ns("map3"))),
      div(class = "map-container map-slot-4", mapModuleUI(ns("map4")))
    )
  )
}

# Map layout module server
mapLayoutModuleServer <- function(id, layout = reactive("layout1")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    layout_zoom_levels <- list(
      layout1 = 5,
      layout2 = 4,
      layout4 = 3
    )
    
    current_zoom <- reactive({
      layout_zoom_levels[[layout()]] %||% 5
    })
    
    # Initialize ALL map servers ONCE
    map1 <- mapModuleServer("map1")
    map2 <- mapModuleServer("map2")
    map3 <- mapModuleServer("map3")
    map4 <- mapModuleServer("map4")
    
    # Update layout via JS (no renderUI)
    observe({
      current_layout <- layout()
      cat("Switching to layout:", current_layout, "\n")
      
      session$sendCustomMessage("updateMapLayout", list(
        containerId = ns("map_container"),
        layout = current_layout
      ))
    }) %>% bindEvent(layout(), ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Update zoom levels
    observe({
      zoom_level <- current_zoom()
      current_layout <- layout()
      
      map1$set_zoom(zoom_level)
      map2$set_zoom(zoom_level)
      map3$set_zoom(zoom_level)
      map4$set_zoom(zoom_level)
      
      # Invalidate hidden maps to refresh when shown
      session$sendCustomMessage("invalidateMaps", list(
        layout = current_layout
      ))
      
    }) %>% bindEvent(layout(), ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Map sync
    observe({
      current_layout <- layout()
      
      map_ids <- switch(
        current_layout,
        "layout1" = character(0),
        "layout2" = c(ns("map1-map"), ns("map2-map")),
        "layout4" = c(ns("map1-map"), ns("map2-map"), ns("map3-map"), ns("map4-map")),
        character(0)
      )
      
      session$sendCustomMessage("syncMaps", list(mapIds = map_ids))
    }) %>% bindEvent(layout(), ignoreNULL = TRUE, ignoreInit = FALSE)
    
    return(list(
      map1 = map1,
      map2 = map2,
      map3 = map3,
      map4 = map4
    ))
  })
}