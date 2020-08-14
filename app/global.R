# Libraries
## App
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
## Data wrangling
library(tidyverse)
library(tidyselect)
library(lubridate)
## Spatial Data Manipulation
library(sf)
library(sp)
library(rgdal)
library(maptools)
library(spatstat)
## Plotting
library(plotly)
library(leaflet)
library(mapview)
library(ggthemes)
# Webscrapping
library(rvest)
library(mapsapi)

# Source functions
source('app/functions_app.R')

######################################### INPUT ###################################
# GADM Geospatial data from GADM, mapping SP.
sp.sf <- readRDS('data/SP.rds')
crime_types <- readRDS('data/crime_types.rds') 
Bairros.choices <- sp.sf %>% as_tibble() %>% select(Bairros)

# Key to Google API
key <- 'AIzaSyAHUwjfucMlHwga81CGjuLm2IS1Eh7SR8A'
######################################### OUTPUT ###################################
