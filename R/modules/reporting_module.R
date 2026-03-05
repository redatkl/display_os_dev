# Page 1 Module: Dashboard
source("R/modules/reporting/sidebar_reporting.R")

reporting_ui <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    sidebar_reporting_ui(ns("sidebar3")),
    

    
  )
}

reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}