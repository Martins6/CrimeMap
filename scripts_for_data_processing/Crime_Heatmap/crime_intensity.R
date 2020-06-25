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

dt.aux <- readRDS('data/crime_by_square/squares_500.rds')

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
plot(yes.cr)

# Kernel Density smoothing
## Diggle bandwith
a1 <- density(yes.cr, sigma = bw.diggle)
a1.1 <- a1
a2 <- density(no.cr, sigma = bw.diggle)

a1 %>% plot()
points(yes.cr)

z1 <- (a1$v)/(a1$v + a2$v)
a1.1$v <- z1
a1.1 %>% plot()

## Spatial Adaptive Density
b1 <- adaptive.density(yes.cr, f=0.05, nrep=5)
b2 <- adaptive.density(no.cr, f=0.05, nrep=5)
b1.1 <- b1
b1 %>% plot()
b2 %>% plot()


z2 <- (b1$v)/(b1$v + b2$v)
b1.1$v <- z2
b1.1 %>% plot()

z2 %>% dim()

# Raster Plot
# Create an empty raster with the same extent and resolution as the Sao Paulo region
predicted_raster <- raster::raster(nrows = nrow(z1),
                             ncols = ncol(z1),
                             ext = raster::extent(sp.sp))
# For some reason, the raster takes the inverse order of rows
predicted_raster[] <- z1[nrow(z1):1,]
plot(predicted_raster)
predicted_raster_in_map <- raster::mask(predicted_raster, sp.sp)
plot(predicted_raster_in_map)

# 3d plot
plot.tibble <- tibble(Mapa = predicted_raster_in_map@data@values %>% as.vector(),
                      x = coordinates(predicted_raster_in_map)[,1],
                      y = coordinates(predicted_raster_in_map)[,2])
## Normal plot
gg = plot.tibble %>% 
  drop_na() %>% 
  ggplot(aes(x = x, y = y)) +
  geom_raster(aes(fill = Mapa)) +
  theme_bw()

## Rayshader
library(rayshader)

plot_gg(gg, multicore = TRUE, width = 6, height=6, fov = 70)
render_depth(focallength=100,focus=0.72)

## Plotly
plotly.matrix <- matrix(data = as.vector(plot.tibble$Mapa), nrow(z2), ncol(z2))
fig <- plotly::plot_ly(z = ~ plotly.matrix)
fig <- fig %>% plotly::add_surface()
fig


########################################## BIVAND METHOD ##########################################
# Following first the example of Bivand

# How many parts to divide the region in each dimension?
h <- 500
# Divinding the region into quadrat or 'little squares'
qcount <- quadratcount(crime.ppp, nx = h, ny = h) %>% 
  as_tibble()

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