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
  name = c("release_location", "gn-7", "s-STD3"),
  lon = c(5.003906, 4.787587, 4.272780513),
  lat = c(51.144779, 51.061848, 51.332135212375)
)

# Convert to sf object
points_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# load raster
study.area.binary.extended <- rast(
  "./data/geo_data/study_area_binary_50m.tif"
) #should be of type SpatRaster

# get.distance.from.source(
#   raster = study.area.binary.extended,
#   XY = points_sf
# )

# #plot raster and points
# mapview(study.area.binary.extended) +
#   mapview(points_sf, col.regions = "blue", layer.name = "Points")

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
#setup vector from 0 to 105 000m, resolution each2 m
target_dis <- seq(0, 105000, by = 10) #105000
end_point <- get.coordinates(
  raster = study.area.binary.extended,
  source = source,
  target_dist = target_dis,
  target_crs = 31370
)

#get coordinates
coords_lat_lon <- st_coordinates(end_point)
coord_lamb <- st_transform(end_point, crs = 31370)
coords_lamb <- st_coordinates(coord_lamb)
#create lookup table
#THE RESOLUTION OF THE LOOKUP TABLE IS RESTRICTED TO THE RESOLUTION OF THE RASTER
#THE MORE RESOLUTION THE RASTER THE LONGER THE RUN FOR RASTERIZING AND CREATING THE LOOKUP TABLE!
lookup_table <- data.frame(
  distance_to_source = end_point$target_dist,
  longitude = st_coordinates(end_point)[, 1],
  latitude = st_coordinates(end_point)[, 2],
  X = coords_lamb[, 1],
  Y = coords_lamb[, 2]
)
write.csv(
  lookup_table,
  "./data/geo_data/lookup_table_50m.csv",
  row.names = FALSE
)

#plot raster begin and end point
mapview(study.area.binary.extended) +
  mapview(points_sf, col.regions = "blue", layer.name = "Start point") +
  mapview(end_point, col.regions = "red", layer.name = "End point")
