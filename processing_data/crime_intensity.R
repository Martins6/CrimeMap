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
  rename(Bairros = NAME_3)

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
sp.sp <- sp.sf %>% as_Spatial()
sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
zone <- 23
sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))

# The point object
crime.ppp %>% summary()
#crime.ppp %>% plot()
# # The region object
sp.sp %>% summary()

# How many parts to divide the region in each dimension?
h <- 100
# Divinding the region into quadrat or 'little squares'
qcount <- quadratcount(crime.ppp, nx = h, ny = h) %>% 
  as_tibble()

qcount

# adapting the tibble
## Auxiliary fun
decomposing <- function(x){
  x %>%
    str_replace('\\[', replacement = '') %>%
    strsplit(',') %>%
    unlist() %>%
    first() %>%
    as.numeric()
}
dt.aux <- qcount %>% 
  #sample_n(1000) %>% 
  mutate(y = unlist(lapply(y, decomposing)),
         x = unlist(lapply(x, decomposing)),
         crime.event = if_else(n > 0,
                               1,
                               0)) %>% 
  select(-n)

dt.aux$crime.event %>% hist()

yes.cr <- dt.aux %>% 
  filter(crime.event == 1) %>% 
  select(x, y, crime.event) %>% 
  SpatialPoints() %>% 
  as('ppp')
no.cr <- dt.aux %>% 
  filter(crime.event == 0) %>% 
  select(x, y, crime.event) %>% 
  SpatialPoints() %>% 
  as('ppp')

yes.cr %>% summary()

no.cr %>% summary()

plot(no.cr)

# Kernel Density smoothing
#bw.diggle(crime.ppp) %>% plot()
a1 <- density(yes.cr, sigma = bw.diggle)
a2 <- density(no.cr, sigma = bw.diggle)



a1 %>% plot()
points(yes.cr)

z1 <- (a1$v)/(a1$v + a2$v)
a1$v <- z1
a1 %>% plot()

b1 <- adaptive.density(yes.cr, f=0.1, nrep=10)
b2 <- adaptive.density(no.cr, f=0.1, nrep=10)
b1 %>% plot()
b2 %>% plot()


z2 <- (b1$v)/(b1$v + b2$v)
b1$v <- z2
b1 %>% plot()

z2 %>% dim()

# 3d plot
fig <- plotly::plot_ly(z = ~z2)
fig <- fig %>% plotly::add_surface()
fig

# Raster Plot
# Create an empty raster with the same extent and resolution as the bioclimatic layers
emp_raster <- raster::raster(nrows = nrow(z2),
                             ncols = ncol(z2),
                             ext = raster::extent(sp.sp))
#emp_raster <- coordinates(emp_raster)
fil_raster@file@toptobottom <- TRUE
fil_raster <- raster::setValues(x = emp_raster, z2)

plot(fil_raster)


z2
sp.sf %>%  ggplot() +
  geom_sf() 

