if (!require(devtools)) {
  install.packages('devtools')
}
library(sf)
library(crawl)
library(ggspatial)
library(mapview)
library(prettymapr)
library(dbscan)
library(adehabitatLT)
library(factoextra)

# Source functions
source("./src/add_segments_function.R")

# Resolution is set in config.R
##########################################################################################
# simple interpolation of the trajectory
##########################################################################################
data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)
sf_points <- st_as_sf(
  data,
  coords = c("deploy_longitude", "deploy_latitude"),
  crs = 4326 # WGS84
) %>%
  st_transform(crs = 31370) #Lambert 72 = 31370

# Extract coordinates back to data frame columns x and y
data$x <- NA
data$y <- NA
data[, c("x", "y")] <- st_coordinates(sf_points)
# cord.dec <- SpatialPoints(
#   data[, c("deploy_longitude", "deploy_latitude")],
#   proj4string = CRS("+proj=longlat")
# )
# test <- spTransform(cord.dec, CRS("+proj=utm +zone=31 +ellps=WGS84"))
# data[, c("x", "y")] <- coordinates(test)

data$middledate <- as.POSIXct(data$arrival + data$residence / 2) #PJ werkt op arrival time en niet op middledate
data$rounded_date <- floor_date(data$middledate, unit = resolution_s)
data$tag_serial_number <- as.character(data$tag_serial_number)

id <- unique(data$tag_serial_number)
#Store data in an object of class "ltraj"
xy <- data[, c("x", "y")]
date <- data$middledate
id <- as.character(data$tag_serial_number)
id_unique <- unique(id)
#data_rounded_date or data_middledate?
traj <- as.ltraj(xy, data$rounded_date, id) #traj[[1]]$dist --> 21 object for which the last one is NA
#t is one hour in seconds

#data$cluster <- NA

inter_all <- redisltraj(traj, u = minutes * 60, type = "time")

# id toevoegen aan elk traject
for (k in 1:length(inter_all)) {
  inter_all[[k]]$id <- id_unique[k]
}
data_inter <- do.call(rbind.data.frame, inter_all) # %>% filter(!is.na(dist))
data_inter$is_original <- NA
valid_ids <- unique(data_inter$id)
data_inter.eel <- split(data_inter, data_inter$id)
data_inter <- data_inter %>%
  rename(
    tag_serial_number = id
  )

data_filtered <- data[data$tag_serial_number %in% valid_ids, ]
data.eel.filtered <- split(data_filtered, data_filtered$tag_serial_number)

for (k in seq_along(data_inter.eel)) {
  inter.temp <- data_inter.eel[[k]]
  data.temp <- data.eel.filtered[[k]]

  original_times <- data.temp$rounded_date
  new_times <- inter.temp$date

  #is_original <- floor_date(new_times, unit = resolution_s) %in% floor_date(original_times, unit = resolution_s) # check if the date in inter.temp is in the original data
  is_original <- new_times %in% original_times

  row_ids <- which(data_inter$tag_serial_number == names(data_inter.eel)[k]) #select the row id's in data_inter of id k

  data_inter$is_original[row_ids] <- as.integer(is_original)
}

# with log but nog recommended, because now skewed to lower speeds
log_dist <- data_inter$dist
not_na_idx <- which(!is.na(log_dist))

not_na_idx <- which(!is.na(data_inter$dist))

data_inter <- left_join(
  data_inter[, c("x", "y", "date", "dist", "dt", "tag_serial_number")],
  data[, setdiff(names(data), c("middledate", "x", "y"))],
  by = c("tag_serial_number" = "tag_serial_number", "date" = "rounded_date")
)

#put values in the data_inter_env$speed_m_s column by filling in all the row above a value with that value

data_inter <- data_inter %>%
  group_by(tag_serial_number) %>%
  mutate(
    speed_m_s = zoo::na.locf(speed_m_s, fromLast = TRUE, na.rm = FALSE),
    distance_to_source_m = zoo::na.approx(distance_to_source_m, na.rm = FALSE),
    deploy_latitude = zoo::na.locf(
      deploy_latitude,
      fromLast = TRUE,
      na.rm = FALSE
    ),
    deploy_longitude = zoo::na.locf(
      deploy_longitude,
      fromLast = TRUE,
      na.rm = FALSE
    ),
    cluster = as.factor(zoo::na.locf(cluster, fromLast = TRUE, na.rm = FALSE)),
    zone = as.factor(zoo::na.locf(zone, fromLast = TRUE, na.rm = FALSE))
  ) %>%
  ungroup()

#######################################################################################################################
# Add interpolation of the distance between two receivers (midpoint)
look_up <- read_csv(
  './data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv',
  show_col_types = FALSE
)

dist_af_splits <- 60363 #zeescheldt afwaartst, zeescheldt opwaarts
#add interpolation_location (distance_to_source)
data_inter <- data_inter %>%
  filter(!is.na(dist)) %>%
  group_by(tag_serial_number) %>%
  mutate(
    interpolation_location = round(
      (distance_to_source_m + lag(distance_to_source_m, 1)) / 2
    )
  ) %>%
  ungroup()

data_inter$interpolation_location[is.na(
  data_inter$interpolation_location
)] <- round(data_inter$distance_to_source_m[is.na(
  data_inter$interpolation_location
)])

# add the segment linked with this location
data_inter$inter_segment <- NA
data_inter$inter_segment <- add_segments(
  look_up,
  data_inter,
  "interpolation_location"
)

# add coordinates of interpolation location
data_inter <- data_inter %>%
  dplyr::select(-c(xcoord_inter, ycoord_inter)) %>%
  left_join(
    look_up %>%
      dplyr::select(distance_to_source, NAAM, xcoord, ycoord),
    by = c(
      "interpolation_location" = "distance_to_source",
      "inter_segment" = "NAAM"
    )
  ) %>%
  rename(xcoord_inter = xcoord, ycoord_inter = ycoord) %>%
  filter(!is.na(xcoord_inter) & !is.na(ycoord_inter))

data_sf <- st_as_sf(
  data_inter,
  coords = c("xcoord_inter", "ycoord_inter"),
  crs = 31370
) %>%
  st_transform(crs = 4326)
data_coords <- cbind(data_inter, st_coordinates(data_sf))
data_inter <- data_coords %>%
  dplyr::select(-inter_latitude, -inter_longitude) %>%
  rename(inter_longitude = X, inter_latitude = Y)

data_inter$station_name[
  data_inter$interpolation_location == 0
] <- "rel_grotenete1"
#save as csv
write_csv(
  data_inter,
  paste("./data/interim/migration_inter_", resolution_s, ".csv", sep = "")
)
