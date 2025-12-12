# Map layout Module
source("R/modules/geo/map.R")


# Map layout module ui
mapLayoutModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/map_layout.css"),
      tags$script(src = "js/map_sync.js")
    ),
  
  
  uiOutput(ns("dynamic_layout"))
  )
}


# Map layout module server
mapLayoutModuleServer <- function(id, layout = reactive("layout1")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize ALL map servers ONCE (not conditionally)
    map1 <- mapModuleServer("map1")
    map2 <- mapModuleServer("map2")
    map3 <- mapModuleServer("map3")
    map4 <- mapModuleServer("map4")
    
    # Generate dynamic UI based on layout
    output$dynamic_layout <- renderUI({
      current_layout <- layout()
      
      cat("Rendering layout:", current_layout, "\n")  # Debug
      
      switch(
        current_layout,
        
        # 1 Map Layout
        "layout1" = {
          div(
            class = "map-layout layout-1",
            mapModuleUI(ns("map1"))
          )
        },
        
        # 2 Maps Layout (side by side)
        "layout2" = {
          div(
            class = "map-layout layout-2",
            div(class = "map-container map-left",
                mapModuleUI(ns("map1"))),
            div(class = "map-container map-right",
                mapModuleUI(ns("map2")))
          )
        },
        
        # 4 Maps Layout (grid)
        "layout4" = {
          div(
            class = "map-layout layout-4",
            div(class = "map-container map-top-left",
                mapModuleUI(ns("map1"))),
            div(class = "map-container map-top-right",
                mapModuleUI(ns("map2"))),
            div(class = "map-container map-bottom-left",
                mapModuleUI(ns("map3"))),
            div(class = "map-container map-bottom-right",
                mapModuleUI(ns("map4")))
          )
        },
        
        # Default fallback
        div(class = "map-layout layout-1",
            mapModuleUI(ns("map1")))
      )
    })
    
    # Return map module references
    return(list(
      map1 = map1,
      map2 = map2,
      map3 = map3,
      map4 = map4
    ))
  })
}