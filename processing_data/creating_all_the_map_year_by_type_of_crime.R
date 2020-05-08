# title: Processing all the data and creating maps in the format of a tibble
# author: Adriel Martins
# date: 04/05/20
# ************************************************************************* #
# ***************** Libraries
# Data manipulation
library(tidyverse)
# Spatial Data Manipulation
library(sf)
library(sp)
library(rgdal)

# ******************* Reading Data
# GADM Geospatial data from GADM, mapping SP.
sp.sf <- readRDS('data/SP.rds')
# Getting the paths for all our data-files
all.crime.files <- list.files('/home/adriel_martins/Documents/CrimeMap/data_pre_processed/SSP')
# Reading Data from the SSP, which was on Kaagle.
crime <- read_csv("data_pre_processed/SSP/BO_2015.csv")

# *********** Fixed objects in the loop
# Calculating bounding box for further wrangling
boundbox.sp <- sp.sf$geometry %>% sf::as_Spatial() %>% bbox()
# Loop




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