# title: Intensity Map!
# author: Adriel Martins
# date: 13/05/20
# ************************************************************************* #
# ***************** Libraries
# Data manipulation
library(tidyverse)
# Spatial Data Manipulation
library(sp)
library(sf)
library(rgdal)
library(spatstat)
library(maptools)
###################### ****************** Reading Data ######################
# GADM Geospatial data from GADM, mapping the whole of Brazil
brazil.sf <- readRDS('data_pre_processed/gadm36_BRA_3_sf.rds')

# Reading Data from the SSP, which was on Kaagle.
crime <- read_csv("data_pre_processed/SSP/BO_2016.csv")
###################### ****************** Data Wrangling ######################
# Filtering our Map of Brazil into the Map of the City of Sao Paulo.
sp.sf <- brazil.sf %>%
  filter(NAME_2 == 'São Paulo') %>%
  select(NAME_3) %>% 
  # Renaming
  rename(Bairros = NAME_3) %>%
  filter(Bairros == 'Se')

# Calculating bounding box for further wrangling
boundbox.sp <- sp.sf$geometry %>% sf::as_Spatial() %>% bbox()

# Let us modify the data as we see fit
# For our purposes, we need the precise location, so let's clean the data without geotag.
crime.t <- crime %>% 
  # Filtering just for the year at hand
  filter(as.character(ANO_BO) == '2016') %>% 
  # Selecting what we want
  select(LATITUDE, LONGITUDE, MES, RUBRICA) %>% 
  # Just because I prefer lower case letters
  `colnames<-`(., str_to_lower(colnames(.))) %>% 
  # Interpreting as double the lat-long coordinates
  mutate(latitude = as.double(latitude),
         longitude = as.double(longitude),
         # Changing the name of each month to it's text counterpart
         mes = lubridate::month(mes, label = T)) %>% 
  # Dropping observations without lon-lat
  drop_na() %>% 
  # Let us exclude the points that are not in the bounding box of the City of São Paulo
  filter(between(latitude, boundbox.sp[2,1], boundbox.sp[2,2]) &
           between(longitude, boundbox.sp[1,1], boundbox.sp[1,2])) %>% 
  # Let us also aggregate crime, by taking out the 'A.I'.
  mutate(rubrica = str_replace_all(rubrica, 'A.I.-', '')) 

# Selecting what type of crime
crime <- crime.t %>% 
  filter(rubrica == 'Roubo (art. 157)') 

# Let us put our crime dataset into a proper spatial format for point patterns
crime.sp <- crime %>% select(longitude, latitude) %>% SpatialPoints()
crime.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
zone <- 23
crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
crime.ppp <- crime.sp %>% as('ppp')
# Scaling to kilometers
crime.ppp$x <- crime.ppp$x - min(crime.ppp$x)
crime.ppp$y <- crime.ppp$y - min(crime.ppp$y)

crime.ppp$window$xrange <- c(min(crime.ppp$x),
                             max(crime.ppp$x))
crime.ppp$window$yrange <- c(min(crime.ppp$y),
                             max(crime.ppp$y))

crime.ppp <- rescale(crime.ppp, 1000, 'kilometers')

# sp.sp <- sp.sf %>% as_Spatial()
# sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# zone <- 23
# sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))

# The point object
crime.ppp %>% summary()
# # The region object
# sp.sp %>% summary()

# Kernel Density smoothing

bw.diggle(crime.ppp) %>% plot()
a <- density(crime.ppp, sigma=0.0003491266)
a <- adaptive.density(crime.ppp, f=0.1, nrep=10)
a %>% plot()
points(crime.ppp)
b <- a$v
fig <- plot_ly(z = ~b)
fig <- fig %>% add_surface()
fig
