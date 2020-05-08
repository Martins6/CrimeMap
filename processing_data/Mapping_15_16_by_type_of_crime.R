# title: Map of every type of crime for the years of 2015 and 2016
# author: Adriel Martins
# date: 04/05/20
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

############ Fixed objects in the loop ##########
# Calculating bounding box for further wrangling
boundbox.sp <- SP$geometry %>% sf::as_Spatial() %>% bbox()

############ Loop for year ##########
for(i in 1:length(all.crime.files)){
  
  path.file <- paste('data_pre_processed/SSP/', all.crime.files[i], sep = '')
  
  crime_i <- read_csv(path.file)
  crime_ano <- crime_i$ANO_BO %>% unique() %>% .[1]
  types_of_crime <- crime_i$RUBRICA %>% unique() %>% str_replace_all('A.I.-', '') 
  
  ############ Loop for type of crime ##########
  for(j in types_of_crime){
    print(c(i, j))
    # *********** Manipulating ##########
    # Let us modify the data as we see fit
    # For our purposes, we need the precise location, so let's clean the data without geotag.
    crime_j <- crime_i %>% 
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
      mutate(rubrica = str_replace_all(rubrica, 'A.I.-', '')) %>% 
      # Filtering to the crime we wish
      filter(rubrica == j)
    
    # Let us put our crime dataset into a proper spatial format
    crime_sp <- crime_j %>% select(longitude, latitude) %>% SpatialPoints()
    crime_sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    # With just point coordinates
    crime_sf <- crime_sp %>% st_as_sf()
    print('Rei Leao')
    # Adding data
    crime_sf$mes <- crime_j$mes
    # Counting how many crimes happened in the year
    SP <- readRDS('data/SP.rds')
    x <- paste('Número do Crime Selecionado em', crime_ano)
    SP$bairros_crime_n <- lengths(st_contains(SP, crime_sf))
    SP <- SP %>%
      rename_at(.vars = vars(bairros_crime_n), .funs = ~x)
    print('Hakunamatata')
    # Counting how many crimes happened just in each month
    order_month <- unique(crime_j$mes) %>% sort()
    for(h in order_month){
      # Selecting the crime_points of each month
      crime_month <- crime_sf %>% filter(mes == h)
      # Counting how many there are in each neighborhood by month
      bairros_crime_n <- lengths(st_contains(SP, crime_month))
      # Binding those two together and changing the name to the respective month
      x <- paste('Número de Crime Selecionado em', h, crime_ano)
      SP$bairros_crime_n <- bairros_crime_n
      SP <- SP %>%
        rename_at(.vars = vars(bairros_crime_n), .funs = ~x)
    }
    print('É lindo de ver')
    # Writing out the file in rds format
    path.file <- paste('data/', 'SPcrimetibble', crime_ano, j,'.rds', sep = '')
    SP %>% as_tibble() %>% select(-geometry) %>% write_rds(path.file)
    
  }# enf of loop for each type of crime
}# end of loop for every year
  

