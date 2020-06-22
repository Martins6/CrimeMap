# title: Map only Robbery in 2015 and 2016
# author: Adriel Martins
# date: 22/05/20
# ************************************************************************* #
###################### ****************** Reading Data ######################
# ***************** Libraries
# Data manipulation
library(tidyverse)
# Spatial Data Manipulation
library(sf)
library(sp)
library(rgdal)

############ Reading Data ##########
# GADM Geospatial data from GADM, mapping SP.
SP <- readRDS('data/SP.rds')
# Getting the paths for all our data-files, just no the SP
all.crime.files <- c('BO_2015.csv', 'BO_2016.csv')