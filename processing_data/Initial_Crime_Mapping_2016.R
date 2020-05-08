# title: Mapping Crime in the Great Region of Sao Paulo City (year of 2016 and 2015)
# author: Adriel Martins
# date: 04/05/20
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
# GADM Geospatial data from GADM, mapping the whole of Brazil
brazil.sf <- readRDS('data_pre_processed/gadm36_BRA_3_sf.rds')

# Reading Data from the SSP, which was on Kaagle.
crime <- read_csv("data_pre_processed/SSP/BO_2015.csv") # read_csv("data_pre_processed/BO_2016.csv")
###################### ****************** Initial Data Exploring ######################
# # Understandig the geo-tag of our crime dataset
# crime %>% 
#   # Interpreting as double the lat-long coordinates, and month as integer
#   mutate(Index = 1: n(),
#          latitude = as.double(latitude),
#          longitude = as.double(longitude),
#          mes = as.integer(mes)) %>% 
#   filter(is.na(latitude) | is.na(longitude)) %>% 
#   summarise(WithoutLatLongCoord = n(),
#             WithLatLongCoord = nrow(crime) - n(),
#             PropWithout = WithoutLatLongCoord/nrow(crime),
#             PropWith = WithLatLongCoord/nrow(crime))
# # Cities where crime was recorded
# crime %>% pull(cidade) %>% str_to_lower() %>% unique()
# # Number of crimes in the city of Sao Paulo
# crime %>% filter(cidade == 'S.PAULO') %>% nrow()
# # Different types of crime in SP and their respective quantity
# crime %>% group_by(rubrica) %>% summarise(Count = n())
# crime$rubrica %>% unique()
###################### ****************** Data Wrangling ######################
# Filtering our Map of Brazil into the Map of the City of Sao Paulo.
sp.sf <- brazil.sf %>%
  filter(NAME_2 == 'São Paulo') %>%
  select(NAME_3) %>% 
  # Renaming
  rename(Bairros = NAME_3)
# Calculating bounding box for further wrangling
boundbox.sp <- sp.sf$geometry %>% sf::as_Spatial() %>% bbox()

# Storing the date of every year
crime_ano <- crime$ANO_BO %>% unique() %>% .[1]
# Let us modify the data as we see fit
# For our purposes, we need the precise location, so let's clean the data without geotag.
crime <- crime %>% 
  # Filtering just for the year at hand
  filter(ANO_BO == crime_ano) %>% 
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

# Let us put our crime dataset into a proper spatial format
crime_sp <- crime %>% select(longitude, latitude) %>% SpatialPoints()
crime_sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# With just point coordinates
crime_sf <- crime_sp %>% st_as_sf()
# Adding the type of crime commited
crime_sf$rubrica <- crime$rubrica
crime_sf$mes <- crime$mes
# Changing the name of our variable for plotting 
SP <- sp.sf

# Counting how many crimes happened in the year
x <- paste('Número de Crimes em', crime_ano)
SP$bairros_crime_n <- lengths(st_contains(SP, crime_sf))
SP <- SP %>% 
  rename_at(.vars = vars(bairros_crime_n), .funs = ~x)

# Counting how many crimes happened just in each month
order_month <- unique(crime$mes) %>% sort()
for(i in order_month){
  # Selecting the crime_points of each month
  crime_month <- crime_sf %>% filter(mes == i)
  # Counting how many there are in each neighborhood by month
  bairros_crime_n <- lengths(st_contains(SP, crime_month))
  # Binding those two together and changing the name to the respective month
  x <- paste('Número de Crimes em', i, crime_ano)
  SP$bairros_crime_n <- bairros_crime_n
  SP <- SP %>% 
    rename_at(.vars = vars(bairros_crime_n), .funs = ~x)
}

# Writing out the file in rds format
path.file <- paste('/home/adriel_martins/Documents/CrimeMap/data', 
                   '/', 'SPcrimetibble', crime_ano, '.rds', sep = '')
SP %>% as_tibble() %>% select(-geometry) %>% write_rds(path.file)
###################### ****************** Data Vizualisation ######################
# Visualizing a sample of our crime data with their coordinates
# crime %>%
#   sample_n(100) %>% 
#   leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng = ~longitude, lat = ~latitude)

# Visualizing Crime by Neighborhood, first attempt at a heatmap
# SP <- merge(SP, SPcrimetibble2015)
# 
# pal = mapviewPalette("mapviewSpectralColors")
# 
# max.val <- max(SP$`Número de Crimes em 2015`)
# min.val <- min(SP$`Número de Crimes em 2015`)
# 
# mapview(SP, zcol = 'Número de Crimes em 2015',
#         col.regions = pal(n = 10), at = seq(min.val, max.val, l = 10))

