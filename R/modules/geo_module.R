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

    
    # Create connection
    # conn <- init_db()
    # onStop(function() dbDisconnect(conn))
    
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
    
    # Render map1
    # observe({
    #   panel <- sidebar_vals$active_panel()
    #   params <- sidebar_vals$map_params$map1[[panel]]
    #   
    #   req(params$indice, params$date)
    #   
    #   rast <- fetch_raster(params$indice, params$temporalite, params$date, conn)
    #   add_raster_layer("map_layout-map1-map", rast, params$indice)
    #   
    # }) %>% bindEvent(sidebar_vals$map_params$map1, ignoreInit = TRUE)
    # 
    # # Render map2
    # observe({
    #   req(selected_layout() %in% c("layout2", "layout4"))
    #   
    #   panel <- sidebar_vals$active_panel()
    #   params <- sidebar_vals$map_params$map2[[panel]]
    #   
    #   req(params$indice, params$date)
    #   
    #   rast <- fetch_raster(params$indice, params$temporalite, params$date, conn)
    #   add_raster_layer("map_layout-map2-map", rast, params$indice)
    #   
    # }) %>% bindEvent(sidebar_vals$map_params$map2, selected_layout(), ignoreInit = TRUE)
    # 
    # # Render map3
    # observe({
    #   req(selected_layout() == "layout4")
    #   
    #   panel <- sidebar_vals$active_panel()
    #   params <- sidebar_vals$map_params$map3[[panel]]
    #   
    #   req(params$indice, params$date)
    #   
    #   rast <- fetch_raster(params$indice, params$temporalite, params$date, conn)
    #   add_raster_layer("map_layout-map3-map", rast, params$indice)
    #   
    # }) %>% bindEvent(sidebar_vals$map_params$map3, selected_layout(), ignoreInit = TRUE)
    # 
    # # Render map4
    # observe({
    #   req(selected_layout() == "layout4")
    #   
    #   panel <- sidebar_vals$active_panel()
    #   params <- sidebar_vals$map_params$map4[[panel]]
    #   
    #   req(params$indice, params$date)
    #   
    #   rast <- fetch_raster(params$indice, params$temporalite, params$date, conn)
    #   add_raster_layer("map_layout-map4-map", rast, params$indice)
    #   
    # }) %>% bindEvent(sidebar_vals$map_params$map4, selected_layout(), ignoreInit = TRUE)
  })
}