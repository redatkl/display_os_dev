# load necessary packages and scripts

# Load required libraries
library(shiny)
library(lubridate)
library(leaflet)
library(rlang)
library(dplyr)
library(sf)

# Source module files
source("R/modules/accueil_module.R")
source("R/modules/data_module.R")
source("R/modules/station_module.R")
source("R/modules/geo_module.R")
source("R/modules/forecast_module.R")
source("R/modules/reporting_module.R")



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

# map utils
source("R/functions/db_config.R")
source("R/functions/map_utils.R")


# Initialize connection db stations map

# Weather stations database connection
db_name_weather = Sys.getenv("DB_NAME_WEATHER")
db_host_weather = Sys.getenv("DB_HOST_WEATHER")
db_user_weather = Sys.getenv("DB_USER_WEATHER")
db_password_weather = Sys.getenv("DB_PASSWORD_WEATHER")

# Initialize connection db stations map
# init_weather_db <- function() {
#   dbConnect(
#     Postgres(),
#     dbname = db_name_weather,
#     host = db_host_weather,
#     port = 5432,
#     user = db_user_weather,
#     password = db_password_weather
#   )
# }

# import maroc shp
maroc <- st_read("data/administrative/Maroc.geojson")