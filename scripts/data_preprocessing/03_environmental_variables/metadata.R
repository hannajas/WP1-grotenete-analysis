# load raw metadata
# preprocessing: add resolution, station coordinates for rainfall measurements, wether a station is located at a receiver
# write to /interim folder
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(wateRinfo)
library(tidyverse)
library(rvest)
library(sf)
# source function
source("./src/add_segments_function.R")

#Load data
data <- read_csv('./data/raw/migration.csv', show_col_types = FALSE)
# Load metadata
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
n_data <- dim(metadata)[1] #number of variables
metadata$resolution <- NA
metadata$station_longitude <- NA
metadata$station_latitude <- NA

#add longitude and latitude of the rainfall stations
stations <- get_stations("rainfall", frequency = "15min") %>%
  filter(station_longitude > 4 & station_latitude > 50.88) %>%
  arrange(station_no)
metadata[metadata$type == "R", ]$station_longitude <- stations$station_longitude
metadata[metadata$type == "R", ]$station_latitude <- stations$station_latitude

for (i in 1:n_data) {
  if (
    !is.na(metadata$resolution_unit[i]) &
      !is.na(metadata$resolution_multiplier[i])
  ) {
    metadata$resolution[i] <- toString(as.period(
      metadata$resolution_multiplier[i],
      metadata$resolution_unit[i]
    ))
  }
}

#add if a station is located at a receiver
receiver <- lapply(1:n_data, function(i) {
  data$station_name[which(
    round(data$distance_to_source_m, digits = 2) ==
      round(metadata$distance_to_source[i], digits = 2)
  )][1]
})
metadata$receiver <- unlist(receiver)


##############################################################################################
#Calculate the distance_to_source for each environmental measurement station (not R)
lookup <- read_csv(
  './data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv',
  show_col_types = FALSE
)
metadata_filter <- filter(
  metadata,
  metadata$type != "R" & metadata$type != "photoperiod"
)
#to sf
metadata_sf <- st_as_sf(
  metadata_filter,
  coords = c("xcoord", "ycoord"),
  crs = 31370
)
lookup_sf <- st_as_sf(
  lookup,
  coords = c("xcoord", "ycoord"),
  crs = 31370
)
#find for each deployment the closest lookup point
dist_matrix <- st_distance(metadata_sf, lookup_sf)
min_indices <- apply(dist_matrix, 1, which.min)
#add distance to source to deployments
metadata$distance_to_source[
  metadata$type != "R" & metadata$type != "photoperiod"
] <- lookup$distance_to_source[min_indices]
metadata$distance <- NA
metadata$distance[
  metadata$type != "R" & metadata$type != "photoperiod"
] <- lookup$distance[min_indices]


##############################################################################################
#Add segments
metadata$segment <- add_segments(lookup, metadata, "distance_to_source")

write.csv(metadata, './data/interim/metadata.csv')
