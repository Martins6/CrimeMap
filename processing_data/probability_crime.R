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
#crime.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# zone <- 23
# crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
crime.ppp <- crime.sp %>% as('ppp')

sp.sp <- sp.sf %>% as_Spatial()
sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# zone <- 23
# sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))

# Constructing the following algorithm:
# 1) Create the Window for your data: a square with the neighborhood that you wish to predict.
# 2) Divide each dimensions into h equal parts, creating h^2 squares, each square with s^2 units.
# 3) Check if there is a crime into each square and atrribute one, if there is, and 0, if there isn't.
# Call that variable Z.
# 4) Fit the logistic model with GLMMM or GLGM to variable Y (assaulted, not assaulted).

# The point object
crime.ppp %>% summary()
# The region object
sp.sp %>% summary()

# How many parts to divide the region in each dimension?
h <- 1000
# Divinding the region into quadrat or 'little squares'
qcount <- quadratcount(crime.ppp, nx = h, ny = h) %>% 
  as_tibble()

#qcount
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
  sample_n(10000) %>% 
  mutate(y = unlist(lapply(y, decomposing)),
         x = unlist(lapply(x, decomposing)),
         crime.event = if_else(n > 0,
                               1,
                               0)) %>% 
  select(-n)
dt.aux$crime.event %>% hist()

########################################### GLGM OR GLMM ####################################
# Fitting GLGM or GLMMM in a Spatial Statistics context
t0 <- Sys.time()
fit.glmm <- spaMM::fitme(crime.event ~ 1 + x + y + Matern(1|x + y),
                         data = dt.aux,
                         family = binomial(link = "logit"),
                         method = 'PQL/L')
t1 <- Sys.time()
print(t1 - t0)

summary(fit.glmm)

resid(fit.glmm)^2 %>% sum()
########################################### GLM + Geostatistic ####################################
# Fitting GLM + Geostatistic
fit.glm <- glm(crime.event ~ 1 + x + y,
                         data = dt.aux,
                         family = binomial(link = "logit"))

fit.glm <- step(fit.glm)


summary(fit.glm)
resid(fit.glm)

res.aux <- dt.aux %>% mutate(Resid = resid(fit.glm),
                             Fitted = fitted(fit.glm))

summary(fit.glm)
resid(fit.glm) %>% abs() %>% sum()
resid(fit.glm)^2 %>% sum()

res.aux %>% 
  ggplot() +
  geom_point(aes(x = x, y = y, colour = Fitted))

########################################### PLOTTING ####################################
# Create an empty raster with the same extent and resolution as the bioclimatic layers
latitude_raster <- longitude_raster <- raster::raster(nrows = 100,
                                              ncols = 100,
                                              ext = raster::extent(sp.sp))

# Change the values to be latitude and longitude respectively
longitude_raster[] <- coordinates(longitude_raster)[,1]
latitude_raster[] <- coordinates(latitude_raster)[,2]

# Now create a final prediction stack of the 4 variables we need
pred_stack <- raster::stack(longitude_raster,
                            latitude_raster)
# Rename to ensure the names of the raster layers in the stack match those used in the model
names(pred_stack) <- c("x", "y")
#plot(pred_stack)

predicted_prevalence_raster <- raster::predict(pred_stack, fit.glm, type = 'response')
# plot(predicted_prevalence_raster)
# lines(sp.sp)
predicted_prevalence_raster_oromia <- raster::mask(predicted_prevalence_raster, sp.sp)
plot(predicted_prevalence_raster_oromia)

