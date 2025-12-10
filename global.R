# load necessary packages and scripts

# Load required libraries
library(shiny)
library(lubridate)

# Source module files
source("R/modules/accueil_module.R")
source("R/modules/data_module.R")
source("R/modules/expert_module.R")
source("R/modules/geo_module.R")



# get the year of today
year_footer = year(Sys.Date())