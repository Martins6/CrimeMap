# title: Final form o Probability/Intensity App
# author: Adriel Martins
# date: 13/05/20
# ************************************************************************* #
# ***************** Libraries
# Data manipulation
library(tidyverse)
# Spatial Data Manipulation
library(sp)
library(sf)
library(maptools)
library(tmap)
library(spatstat)
# For webscrapping
library(rvest)
library(stringr)
###################### ****************** Reading Data ######################
# GADM Geospatial data from GADM, mapping the whole of Brazil
brazil.sf <- readRDS('data_pre_processed/gadm36_BRA_3_sf.rds')

# Reading Data from the SSP, which was on Kaagle.
crime <- read_csv("data_pre_processed/SSP/BO_2016.csv")
###################### ****************** Global Data Wrangling ######################
# Filtering our Map of Brazil into the Map of the City of Sao Paulo.
sp.sf <- brazil.sf %>%
  filter(NAME_2 == 'São Paulo') %>%
  select(NAME_3) %>% 
  # Renaming
  rename(Bairros = NAME_3) 

sp.sf$Bairros

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
# Transforming to UTM coordinates
zone <- 23
crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
crime.sf <- crime.sp %>% st_as_sf()

# Also transforming the desired region
sp.sp <- sp.sf %>% as_Spatial()
sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
zone <- 23
sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
sp.sf <- sp.sp %>% st_as_sf()

# Plot-checking
tm_shape(sp.sf) + 
  tm_fill() +
  tm_shape(crime.sf[sample(1:nrow(crime.sf), 1000),]) +
  tm_dots()

##################################### DESCRIPTIVE RISK ################################
# Collecting the population of the municipalities in Sao Paulo
webpage <- read_html('https://pt.wikipedia.org/wiki/Lista_dos_distritos_de_S%C3%A3o_Paulo_por_popula%C3%A7%C3%A3o')

municip_html <- html_nodes(x = webpage, 'table.wikitable tbody tr td:nth-child(2) a')
municip_name_data <- html_text(municip_html)
pop_html <- html_nodes(x = webpage, 'table.wikitable tbody tr td:nth-child(3)')
pop_data <- html_text(pop_html)
# Joining them in a tibble
cleaning_bairros <- function(x){
  
  a <- stringi::stri_trans_general(str = x, 
                                   id = "Latin-ASCII")
  b <- str_to_lower(a)
  d <- str_squish(b)
  return(d)
}

munic_pop <- tibble(Bairros = municip_name_data,
                    Pop = pop_data) %>% 
  mutate(Bairros = cleaning_bairros(Bairros),
         Pop = 1000 * as.double(str_replace(Pop, '\n', '')))

sp.sf[10,1] <- 'Brasilândia'
  sp.sf[76,1] <- 'São Miguel Paulista'
sp.sf[55,1] <- 'Bom Retiro'


hey <- st_contains(sp.sf, crime.sf) %>% 
  lengths()
sp.sf <- sp.sf %>% 
  mutate(Crime.count = hey,
         Bairros = cleaning_bairros(Bairros)) %>% 
  full_join(munic_pop) %>% 
  mutate(Risco = Crime.count/(Pop - Crime.count)) %>% 
  mutate(Risco = round(Risco, digit = 3))
SP
sp.sf %>% mapview::mapview(zcol = 'Risco', pop = NULL)
##################################### RISK MODEL ################################

# Putting into the format of the 'ppp'
sp.sp <- as(sp.sf, "Spatial")
sp.sp@proj4string <- CRS(as.character(NA))
# Window of the points
window <- maptools::as.owin.SpatialPolygons(sp.sp)
# Pure crime coordinates
crime.coord <- matrix(unlist(crime.sf$geometry), ncol = 2, byrow = T)
# Checking points outside the desired region
inside.sp <- inside.owin(crime.coord[,1],
            crime.coord[,2],
            w = window)
# Selecting only those that fall within
crime.coord <- crime.coord[inside.sp,]
# Creating the ppp data
crime.ppp <- ppp(x = crime.coord[,1], y = crime.coord[,2],
                            window = window, check = T)
# Plot-checking
#crime.ppp %>% plot()
# Checking if there are multiplied points
#any(duplicated(crime.ppp))
# How much?
#sum(multiplicity(crime.ppp) > 1)
# Jitter it, so we don't lose the info
crime.ppp <- rjitter(crime.ppp, retry=TRUE, nsim=1, drop=TRUE)
crime.ppp %>% summary()


crime.ppp <- rescale(crime.ppp, 1000, 'kilometers')
###################### ******************* Kernel Density Estimation ######################
# Methods for bandwidth
# bw.ppl(crime.ppp)
# bw.diggle(crime.ppp)
# Kernel estimates
den.dig <- density(crime.ppp, sigma = bw.diggle, edge = T)
plot(den.dig, main = '')
crime.ppp %>% plot()

crime.ppp %>% plot()
sp.sf$Bairros


