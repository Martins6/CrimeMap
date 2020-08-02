# title: Probability Heatmap Map! (repetition intended)
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
# Spatial Data Modelling
library(gstat)
library(geoR)
###################### ****************** Reading Data ######################
# GADM Geospatial data from GADM, mapping the whole of Brazil
brazil.sf <- readRDS('data_pre_processed/gadm36_BRA_3_sf.rds')
###################### ****************** Data Wrangling ######################
# Filtering our Map of Brazil into the Map of the City of Sao Paulo.
sp.sf <- brazil.sf %>%
  filter(NAME_2 == 'São Paulo') %>%
  select(NAME_3) %>% 
  # Renaming
  rename(Bairros = NAME_3)

# Calculating bounding box for further wrangling
boundbox.sp <- sp.sf$geometry %>% sf::as_Spatial() %>% bbox()

sp.sp <- sp.sf %>% as_Spatial()
sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# zone <- 23
# sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))

# Reading our square map
dt.aux <- read_rds('data/crime_by_square/year2015squares_100.rds') 
dt.aux$crime.event %>% hist()
dt.aux %>% 
  ggplot() +
  geom_tile(aes(x, y, fill = crime.event))

########################################### GLGM OR GLMM ####################################
# # # Fitting GLGM or GLMMM in a Spatial Statistics context
# dt.fit <- dt.aux %>% sample_n(100)
# t0 <- Sys.time()
# fit.glmm <- spaMM::fitme(crime.event ~ Matern(1|x + y),
#                          data = dt.fit,
#                          family = binomial(link = "logit"),
#                          method = 'PQL/L')
# t1 <- Sys.time()
# print(t1 - t0)
# fitted_vec <- as.vector(predict(fit.glmm, newdata = hello))
# 
# ?spaMM::fitme
# 
# summary(fit.glmm)
# resid(fit.glmm)^2 %>% sum()
# 
# res.aux %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, colour = Resid)) +
#   scale_colour_gradient(low = 'green', high = 'red')
# 
# dt.aux %>%
#   mutate(fitted_val = fitted_vec) %>% 
#   ggplot() +
#   geom_tile(aes(x,y, fill = fitted_val))
# ########################################### GLM + Geostatistic - Boosting ###############################
dt.fit <- dt.aux #%>% sample_n(100)
############### ************* GLM #############
# Fitting GLM + Geostatistic
fit.glm <- glm(crime.event ~ 1,
                         data = dt.aux,
                         family = binomial(link = "logit"))
fit.glm <- step(fit.glm)
summary(fit.glm)
res.aux <- dt.aux %>% mutate(Resid = dt.aux$crime.event - fitted(fit.glm),
                             Fitted = fitted(fit.glm))

res.aux$Resid %>% abs() %>% mean()
res.aux$Resid^2 %>% mean()

res.aux %>%
  ggplot() +
  geom_point(aes(x = x, y = y, colour = Resid)) +
  scale_colour_gradient(low = 'green', high = 'red')

############### ************* Covariance Model #############
source('/home/adriel_martins/Documents/rcodes/[R]Spatial_Codes/Geostatistics/my_codes/LongLat_UTM.R')
source('/home/adriel_martins/Documents/rcodes/[R]Spatial_Codes/Geostatistics/my_codes/Kriging.R')
source('/home/adriel_martins/Documents/rcodes/[R]Spatial_Codes/Geostatistics/my_codes/Variogram_Geodesic_Fitting_Function.R')
source('/home/adriel_martins/Documents/rcodes/[R]Spatial_Codes/Geostatistics/my_codes/Cross_Validation_Geodesic.R')
source('/home/adriel_martins/Documents/rcodes/[R]Spatial_Codes/Geostatistics/my_codes/autofitGeodesic.R')

oz <-
  res.aux %>%
  rename(value = Resid,
         lat = x,
         long = y) %>%
  select(long, lat, value) 

# Transforming to SpatialDataFrame
oz.gstat <- oz %>% `coordinates<-`(c('long', 'lat'))
oz.gstat <- LongLatToUTM(sp_dt = oz.gstat, zone = 23)
# Transforming to UTM coord
oz.utm <- cbind(oz.gstat@coords, oz.gstat@data) %>%
  as_tibble() %>%
  rename(x = lat,
         y = long) %>%
  `coordinates<-`(c('y', 'x'))
# Our empirical variogram
v <- autofit.Variogram.Geodesic(oz)
v$plot
var.fit <- v$variogram

# Just kriging with the observed values to the observed values
inside.krige <- ordinary_kriging(oz, var.fit,
                                 distance = 'haversine',
                                 df.coord = oz[c('lat', 'long')])
########################################### PLOTTING ####################################
# Create an empty raster with the same extent and resolution as the bioclimatic layers
latitude_raster <- longitude_raster <- raster::raster(nrows = 100,
                                              ncols = 100,
                                              ext = raster::extent(sp.sp))

# Change the values to be latitude and longitude respectively
longitude_raster[] <- coordinates(longitude_raster)[,1]
latitude_raster[] <- coordinates(latitude_raster)[,2]

plot(longitude_raster)
# Now create a final prediction stack of the 4 variables we need
pred_stack <- raster::stack(longitude_raster,
                            latitude_raster)
# Rename to ensure the names of the raster layers in the stack match those used in the model
names(pred_stack) <- c("x","y")

plot(pred_stack)
predicted_raster <- raster::predict(pred_stack, fit.glmm)
plot(predicted_raster)
# ############## ************* Kriging #############
# Modelling the spatial residuals
# krige_coord <- expand.grid(seq(predicted_raster@extent@ymin, predicted_raster@extent@ymax, l = 100),
#                             seq(predicted_raster@extent@xmin, predicted_raster@extent@xmax, l = 100)) %>%
#   as_tibble() %>% 
#   rename(long = Var1,
#          lat = Var2)
# # Just kriging with the observed values to the observed values
# inside.krige <- ordinary_kriging(oz, var.fit,
#                                  distance = 'haversine',
#                                  df.coord = krige_coord)

predicted_raster_in_map <- raster::mask(predicted_raster, sp.sp)
plot(predicted_raster_in_map)

