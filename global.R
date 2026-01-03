# load necessary packages and scripts

# Load required libraries
library(shiny)
library(lubridate)
library(leaflet)
library(rlang)

# Source module files
source("R/modules/accueil_module.R")
source("R/modules/data_module.R")
source("R/modules/expert_module.R")
source("R/modules/geo_module.R")



# get the year of today
year_footer = year(Sys.Date())


# libraries to connect the db
library(terra)
library(RPostgres)
library(DBI)
library(raster)
library(rpostgis)

# connect to the database
readRenviron(".Renviron")
db_name = Sys.getenv("DB_NAME")
db_host = Sys.getenv("DB_HOST")
db_user = Sys.getenv("DB_USER")
db_password = Sys.getenv("DB_PASSWORD")

# Initialize connection pool
init_db <- function() {
  dbConnect(
   Postgres(),
    dbname = db_name,
    host = db_host,
    port = 5432,
    user = db_user,
    password = db_password
  )
}

source("R/functions/db_config.R")