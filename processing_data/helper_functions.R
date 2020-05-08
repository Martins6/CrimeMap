# title: Functios to help process geospatial data
# author: Adriel Martins
# date: 08/05/20
# ************************************************************************* #
# Libraries
## Data manipulation
library(tidyverse)
## Spatial Data Manipulation
library(sf)
library(sp)
## Plotting Data
library(rgdal)
library(leaflet)
library(mapview)
###################### ****************** Reading Data ######################
# Reading example data
crime <- readRDS("~/Documents/CrimeMap/data/SPcrimetibble2016.rds")
SP <- readRDS("~/Documents/CrimeMap/data/SP.rds")
###################### ****************** Function ######################
# Function to put every column of a tibble inside a sf object.
a <- merge(SP, crime, all = TRUE)









