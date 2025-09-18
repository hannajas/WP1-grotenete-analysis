library("sf")
library("raster")
library("mapview")
library("leaflet")
library(tidyverse)
library(dplyr)
library(terra)

source("./src/coordinate_to_river1D_function.R")
source("./src/distance_from_source_to_coordinate_function.R")


#########################################################################################################################
#get distance from source
#########################################################################################################################
original_projection <- 4326
coordinate_epsg <- 32631
#write some point to an sf file (releaselocation:51.144779, 5.003906), random other point: 51.061848, 4.787587 (gn-7)
coords <- data.frame(
  name = c("release_location", "gn-7"),
  lon = c(5.003906, 4.787587),
  lat = c(51.144779, 51.061848)
)

# Convert to sf object
points_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# load raster
study.area.binary.extended <- rast(
  "./data/geo_data/study_area_binary.tif"
) #should be of type SpatRaster

get.distance.from.source(
  raster = study.area.binary.extended,
  XY = points_sf
)

#plot raster and points
mapview(study.area.binary.extended) +
  mapview(points_sf, col.regions = "blue", layer.name = "Points")


#########################################################################################################################
#get coordinates
#########################################################################################################################

coords <- data.frame(
  name = c("release_location"),
  lon = c(5.003906),
  lat = c(51.144779)
)

# Convert to sf object
source <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

#return them in the right coordinate system
end_point <- get.coordinates(
  raster = rast_spat,
  source = source,
  target_dist = 21257.31,
  target_crs = 4326
)

#plot raster begin and end point
mapview(study.area.binary.extended) +
  mapview(points_sf, col.regions = "blue", layer.name = "Start point") +
  mapview(end_point, col.regions = "red", layer.name = "End point")
