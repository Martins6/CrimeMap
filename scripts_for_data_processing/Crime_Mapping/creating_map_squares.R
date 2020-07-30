# title: Probability Map!
# author: Adriel Martins
# date: 04/05/20
# ************************************************************************* #
# ***************** Libraries
# Data manipulation
library(tidyverse)
# Spatial Data Manipulation
library(sp)
library(sf)
library(rgdal)
#library(raster)
library(spatstat)
library(maptools)
library(splancs)
###################### ****************** Reading Data ######################
setwd('~/Documents/CrimeMap/')
# GADM Geospatial data from GADM, mapping the whole of Brazil
brazil.sf <- readRDS('data_pre_processed/gadm36_BRA_3_sf.rds')

for (year in c(2016, 2015)) {
  # Reading Data from the SSP, which was on Kaagle.
  crime <-
    read_csv(paste("data_pre_processed/SSP/BO_", year, ".csv", sep = ''))
  ###################### ****************** Data Wrangling ######################
  # Filtering our Map of Brazil into the Map of the City of Sao Paulo.
  sp.sf <- brazil.sf %>%
    filter(NAME_2 == 'São Paulo') %>%
    select(NAME_3) %>%
    # Renaming
    rename(Bairros = NAME_3)
  
  # Calculating bounding box for further wrangling
  boundbox.sp <- sp.sf$geometry %>% sf::as_Spatial() %>% bbox()
  
  # Let us modify the data as we see fit
  # For our purposes, we need the precise location, so let's clean the data without geotag.
  crime.t <- crime %>%
    # Filtering just for the year at hand
    filter(as.character(ANO_BO) == as.character(year)) %>%
    # Selecting what we want
    select(LATITUDE, LONGITUDE, MES, RUBRICA) %>%
    # Just because I prefer lower case letters
    `colnames<-`(., str_to_lower(colnames(.))) %>%
    # Interpreting as double the lat-long coordinates
    mutate(
      latitude = as.double(latitude),
      longitude = as.double(longitude),
      # Changing the name of each month to it's text counterpart
      mes = lubridate::month(mes, label = T)
    ) %>%
    # Dropping observations without lon-lat
    drop_na() %>%
    # Let us exclude the points that are not in the bounding box of the City of São Paulo
    filter(
      between(latitude, boundbox.sp[2, 1], boundbox.sp[2, 2]) &
        between(longitude, boundbox.sp[1, 1], boundbox.sp[1, 2])
    ) %>%
    # Let us also aggregate crime, by taking out the 'A.I'.
    mutate(rubrica = str_replace_all(rubrica, 'A.I.-', ''))
  
  # Selecting what type of crime
  crime <- crime.t %>%
    filter(rubrica == 'Roubo (art. 157)')
  
  # Let us put our crime dataset into a proper spatial format for point patterns
  crime.sp <-
    crime %>% select(longitude, latitude) %>% SpatialPoints()
  #crime.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
  crime.ppp <- crime.sp %>% as('ppp')
  # zone <- 23
  # crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
  #crime.ppp <- crime.sp %>% as('ppp')
  
  sp.sp <- sp.sf %>% as_Spatial()
  sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
  # zone <- 23
  # sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
  
  ########################################## CREATING OUR SQUARES MAP ###################################
  
  # Constructing the following algorithm:
  # 1) Create the Window for your data: a square with the neighborhood that you wish to predict.
  # 2) Divide each dimensions into h equal parts, creating h^2 squares, each square with s^2 units.
  # 3) Check if there is a crime into each square and atrribute one, if there is, and 0, if there isn't.
  # Call that variable Z.
  # 4) Fit the logistic model with GLMMM or GLGM to variable Y (assaulted, not assaulted).
  
  # # The point object
  # crime.ppp %>% summary()
  # # The region object
  # sp.sp %>% summary()
  
  for (i in 1:5) {
    # How many parts to divide the region in each dimension?
    h <- 100 * i
    # Divinding the region into quadrat or 'little squares'
    qcount <- quadratcount(crime.ppp, nx = h, ny = h) %>%
      as_tibble()
    
    sp.sp
    
    #qcount
    # adapting the tibble
    ## Auxiliary fun
    decomposing <- function(x) {
      x %>%
        str_replace('\\[', replacement = '') %>%
        strsplit(',') %>%
        unlist() %>%
        first() %>%
        as.numeric()
    }
    
    dt.aux <- qcount %>%
      mutate(
        y = unlist(lapply(y, decomposing)),
        x = unlist(lapply(x, decomposing)),
        crime.event = if_else(n > 0,
                              1,
                              0)
      )
    
    dt.aux %>% write_rds(str_c(
      c(
        'data/crime_by_square/',
        'year',
        year,
        'squares_',
        h,
        '.rds'
      ),
      collapse = ''
    ))
  }
}
