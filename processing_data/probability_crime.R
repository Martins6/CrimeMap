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
library(splancs)
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
# zone <- 23
# crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
#crime.ppp <- crime.sp %>% as('ppp')

sp.sp <- sp.sf %>% as_Spatial()
sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
# zone <- 23
# sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))

########################################## BIVAND METHOD ##########################################
# Following first the example of Bivand

# How many parts to divide the region in each dimension?
h <- 1000
# Divinding the region into quadrat or 'little squares'
qcount <- quadratcount(crime.ppp, nx = h, ny = h) %>% 
  as_tibble()

sp.sp

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
  sample_n(1000) %>% 
  mutate(y = unlist(lapply(y, decomposing)),
         x = unlist(lapply(x, decomposing)),
         crime.event = if_else(n > 0,
                               1,
                               0)) %>% 
  select(-n)

dt.aux$crime.event %>% hist()
?spkernel2d

data(bodmin)
plot(bodmin$poly, asp=1, type="n")
image(kernel2d(as.points(bodmin), bodmin$poly, h0=2, nx=100, ny=100), 
      add=TRUE, col=terrain.colors(20))
pointmap(as.points(bodmin), add=TRUE)
polymap(bodmin$poly, add=TRUE)
bodmin.xy <- coordinates(bodmin[1:2])
apply(bodmin$poly, 2, range)
grd1 <- GridTopology(cellcentre.offset=c(-5.2, -11.5), cellsize=c(0.2, 0.2), cells.dim=c(75,100))
k100 <- spkernel2d(bodmin.xy, bodmin$poly, h0=1, grd1)
k150 <- spkernel2d(bodmin.xy, bodmin$poly, h0=1.5, grd1)
k200 <- spkernel2d(bodmin.xy, bodmin$poly, h0=2, grd1)
k250 <- spkernel2d(bodmin.xy, bodmin$poly, h0=2.5, grd1)
df <- data.frame(k100=k100, k150=k150, k200=k200, k250=k250)
kernels <- SpatialGridDataFrame(grd1, data=df)
spplot(kernels, checkEmptyRC=FALSE, col.regions=terrain.colors(16), cuts=15)


library(rgdal)
spasthma <- readOGR(".", "spasthma")
spbdry <- readOGR(".", "spbdry")
spsrc <- readOGR(".", "spsrc")
sproads <- readOGR(".", "sproads")

library(maptools)
sG <- Sobj_SpatialGrid(spbdry, maxDim = 50)$SG
gt <- slot(sG, "grid")

pbdry <- slot(slot(slot(spbdry, "polygons")[[1]], "Polygons")[[1]], "coords")

library(splancs)
cases <- spasthma[spasthma$Asthma == "case", ]
ncases <- nrow(cases)
controls <- spasthma[spasthma$Asthma == "control", ]
ncontrols <- nrow(controls)
kcases <- spkernel2d(cases, pbdry, h0 = bwasthma, gt)
kcontrols <- spkernel2d(controls, pbdry, h0 = bwasthma, gt)

bwasthmap <- 0.125
lambda1 <- spkernel2d(cases, pbdry, h0 = bwasthmap, gt)
lambda0 <- spkernel2d(controls, pbdry, h0 = bwasthmap, gt)
lambda1 <- lambda1[idxna]
lambda0 <- lambda0[idxna]
spkratio$prob <- lambda1/(lambda1 + lambda0)
is.na(spkratio$prob) <- !is.finite(spkratio$prob)
########################################## MANUALLY CALCULATING ###################################

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

sp.sp

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
  sample_n(1000) %>% 
  mutate(y = unlist(lapply(y, decomposing)),
         x = unlist(lapply(x, decomposing)),
         crime.event = if_else(n > 0,
                               1,
                               0)) %>% 
  select(-n)

dt.aux$crime.event %>% hist()

########################################### GLGM OR GLMM ####################################
# # # Fitting GLGM or GLMMM in a Spatial Statistics context
# t0 <- Sys.time()
# fit.glmm <- spaMM::fitme(crime.event ~ 1 + x + y + Matern(1|x + y),
#                          data = dt.aux,
#                          family = binomial(link = "logit"),
#                          method = 'PQL/L')
# t1 <- Sys.time()
# print(t1 - t0)
# 
# summary(fit.glmm)
# 
# resid(fit.glmm)^2 %>% sum()
########################################### GLM + Geostatistic ####################################
# ############## ************* GLM #############
# Fitting GLM + Geostatistic
# fit.glm <- glm(crime.event ~ 1 + x + y,
#                          data = dt.aux,
#                          family = binomial(link = "logit"))
# 
# fit.glm <- step(fit.glm)
# 
# 
# summary(fit.glm)
# resid(fit.glm)
# 
# res.aux <- dt.aux %>% mutate(Resid = resid(fit.glm),
#                              Fitted = fitted(fit.glm))
# 
# summary(fit.glm)
# resid(fit.glm) %>% abs() %>% sum()
# resid(fit.glm)^2 %>% sum()
# 
# res.aux %>%
#   ggplot() +
#   geom_point(aes(x = x, y = y, colour = Fitted))
# 
# res.aux
# ############## ************* Covariance Model #############
# oz <- 
#   res.aux %>%
#   rename(value = Resid,
#          lat = x,
#          long = y) %>% 
#   select(lat, long, value) 
# 
# oz.geo <- oz %>%
#   as.geodata()
# # Variogram with Geodesic Distance
# v <- variog.geodesic(oz.geo, max.dist = 30000)
# plot(v, main = 'Lat-Long com distância de haversine',
#      xlab = 'Distância', ylab = 'Semivariância')
# 
# # Modelling Variogram
# ols.fit <- variofit(v, cov.model = 'matern', weights = "equal")
# 
# # CHANGING TO GSTAT VARIOGRAM FORM
# var.fit <- vgm(psill = ols.fit$cov.pars[1],
#                model = "Gau",
#                range = ols.fit$cov.pars[2],
#                nugget = ols.fit$nugget,
#                cutoff = ols.fit$max.dist)
# # Plotting to visually check the model 
# plot(v, main = 'Lat-Long com distância de haversine',
#      xlab = 'Distância', ylab = 'Semivariância')
# lines(variogramLine(var.fit, maxdist = ols.fit$max.dist))
# # Just to check the covariance function
# variogramLine(var.fit, maxdist = ols.fit$max.dist, covariance = T) %>% plot()

########################################### PLOTTING ####################################
# Create an empty raster with the same extent and resolution as the bioclimatic layers
latitude_raster <- longitude_raster <- raster::raster(nrows = 128,
                                              ncols = 128,
                                              ext = raster::extent(sp.sp))

# Change the values to be latitude and longitude respectively
longitude_raster[] <- coordinates(longitude_raster)[,1]
latitude_raster[] <- coordinates(latitude_raster)[,2]



longitude_raster@data@values <- z2


plot(longitude_raster)
# Now create a final prediction stack of the 4 variables we need
pred_stack <- raster::stack(longitude_raster,
                            latitude_raster)
# Rename to ensure the names of the raster layers in the stack match those used in the model
names(pred_stack) <- c("x", "y")

plot(pred_stack)
#plot(pred_stack)
predicted_raster <- raster::predict(pred_stack, fit.glm, type = 'response')
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

plot(predicted_raster)
# lines(sp.sp)
predicted_raster_in_map <- raster::mask(predicted_raster, sp.sp)
plot(predicted_raster_in_map)
