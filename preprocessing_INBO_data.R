library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/calculate_speed_function.R")

# Upload distance matrix
distance_matrix <- read.csv(
  "./data/external/distancematrix_2019_grotenete.csv",
  row.names = 1,
  check.names = FALSE
)

# Upload dataset
data <- read_csv('./data/raw/migration.csv')
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

#######################################################################################################################
# Recalculate the smooth eel track (stop timelimit = 1 hour for original preprocessing)
#######################################################################################################################
station_oud <- "random"
# replace in dataframe data in column tag_serial_number "s18a" tot "A"
data$station_name <- gsub("s-8a", "s-8", data$station_name)
data$station_name <- gsub("s-9a", "s-9", data$station_name)
#data$station_name <- gsub("s-10a", "s-10", data$station_name)
data$station_name <- gsub("s-11", "s-12", data$station_name)
for (i in 1:1081) {
  #print(paste(i))
  #print(paste(data$station_name[i+1]))
  if (
    (data$station_name[i] == data$station_name[i + 1] &
      data$tag_serial_number[i] == data$tag_serial_number[i + 1])
  ) {
    station_oud <- data$station_name[i]
    while (data$station_name[i + 1] == station_oud) {
      data$departure[i] <- data$departure[i + 1]
      data$detections[i] <- data$detections[i] + data$detections[i + 1]
      data <- data[-(i + 1), ]
    }
  }
}

#######################################################################################################################
#recalculate the distance_to_source (resolution 1m instead of 50 m for the distance matrix)
#######################################################################################################################

deployments <- read_csv(
  './data/geo_data/deployments_distance_to_source.csv',
  show_col_types = FALSE
) #probleem --> release locatie niet hierin
data <- left_join(
  data %>% dplyr::select(-distance_to_source_m),
  deployments %>% dplyr::select(station_name, distance_to_source_m),
  by = "station_name"
)

#recalculate the toal distance
data <- data %>%
  group_by(tag_serial_number) %>%
  arrange(arrival, .by_group = TRUE) %>%
  mutate(
    total_distance_m = distance_to_source_m - lag(distance_to_source_m)
  ) %>%
  ungroup()


#######################################################################################################################
# Calculate the alternative speed
#######################################################################################################################
residency_list <- split(data, f = data$tag_serial_number)
speed_list <- lapply(residency_list, function(x) {
  movementSpeeds(x, distance_matrix)
})
speed <- plyr::ldply(speed_list, data.frame)
speed$.id <- NULL

#######################################################################################################################
# calculate wheter the migration was downstream
#######################################################################################################################
data_list <- split(speed, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$seg_id <- paste(dplyr::lag(x$station_name), x$station_name, sep = "_")
  x$downstream <- ifelse(
    x$distance_to_source_m > dplyr::lag(x$distance_to_source_m),
    TRUE,
    FALSE
  )
  cond <- (x$station_name == "s-7" |
    x$station_name == "s-6" |
    x$station_name == "s-5" |
    x$station_name == "s-4c" |
    x$station_name == "s-4b" |
    x$station_name == "s-4a")
  x$downstream[(cond)] <- ifelse(
    x$distance_to_source_m[(cond)] > dplyr::lag(x$distance_to_source_m[(cond)]),
    FALSE,
    TRUE
  )
  x$downstream[x$seg_id == "me-7-2b_s-7"] <- FALSE
  x$downstream[x$seg_id == "s-7_me-7-2b"] <- FALSE
  x$downstream[x$seg_id == "s-7_s-8"] <- TRUE
  x$downstream[x$seg_id == "s-7_s-9"] <- TRUE
  return(x)
})
data <- plyr::ldply(data_temp, data.frame)


#######################################################################################################################
# calculate 'downstream_migration'
#######################################################################################################################
speed_threshold <- 0.01
data$downstream_migration <- (data$downstream == TRUE &
  data$speed_m_s >= speed_threshold) # | (data$downstream==TRUE & !(lag(data$migration_speed) >= 30*data$migration_speed)) | (data$downstream==TRUE & !(lead(data$migration_speed) <= 30*data$migration_speed))
data$downstream_migration[
  data$downstream == TRUE & (lead(data$speed_m_s) * 10 <= data$speed_m_s)
] <- FALSE
data$downstream_migration <- ifelse(
  (data$downstream == TRUE &
    (lag(data$migration_speed) >= 30 * data$migration_speed)),
  FALSE,
  TRUE
)

#######################################################################################################################
#add column to devide the study area in a tidal, transition and non-tidal area
grenswaardes_distance_to_source <- c(28833.23349, 43106.96) #boundaries between de different zones (looked at receivers distance_to_source and then ruler in QGIS)
data$zone <- "transition"
data$zone[
  data$distance_to_source_m < grenswaardes_distance_to_source[1]
] <- "non-tidal"
data$zone[
  data$distance_to_source_m > grenswaardes_distance_to_source[2]
] <- "tidal"


#######################################################################################################################
# Add interpolation of the distance between two receivers (midpoint)
data <- data %>%
  group_by(tag_serial_number) %>%
  mutate(
    interpolation_location = round(
      (distance_to_source_m + lag(distance_to_source_m, 1)) / 2
    )
  ) %>%
  ungroup()
data$interpolation_location[is.na(data$interpolation_location)] <- round(data$distance_to_source_m[is.na(data$interpolation_location)])

#######################################################################################################################
# add column to divide in segments: gn, rup, zes_up, zes_down
# summate all the receivers in the Zeeschelde downstream the confluence of the Rupel
zes_down <- c(
  "s-8",
  "s-8a",
  "s-9",
  "s-9a",
  "s-10",
  "s-10a",
  "ak-41",
  "s-11",
  "s-12",
  "s-STD3"
)
# add all station names starting with ws
ws_stations <- grep("^ws-", unique(data$station_name), value = TRUE)
zes_down <- c(zes_down, ws_stations)

#upload lookup table
look_up <- read_csv(
  './data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv',
  show_col_types = FALSE
)
boundaries <- look_up %>%
  group_by(NAAM) %>%
  summarise(max_distance = max(distance)) %>%
  ungroup()
data$river_segment <- "rup"
data$inter_segment <- "rup"
data$river_segment[
  data$distance_to_source_m < boundaries$max_distance[1]
] <- "gn"
data$inter_segment[
  data$interpolation_location < boundaries$max_distance[1]
] <- "gn"
data$river_segment[
  data$distance_to_source_m > boundaries$max_distance[2]
] <- "zes"
data$inter_segment[
  data$interpolation_location > boundaries$max_distance[2]
] <- "zes"

data <- data %>%
  mutate(
    river_segment = case_when(
      station_name %in% zes_down ~ "zes_down",
      river_segment == "zes" ~ "zes_up",
      TRUE ~ river_segment
    ),
    inter_segment = case_when(
      #klopt niet, je zit niet bij het station!
      inter_segment == "zes" & station_name %in% zes_down ~ "zes_down",
      inter_segment == "zes" ~ "zes_up",
      TRUE ~ inter_segment
    )
  )


#######################################################################################################################
# add coordinates to interpolation location
data <- data %>%
  left_join(
    look_up %>%
      dplyr::select(distance_to_source, NAAM, xcoord, ycoord),
    by = c(
      "interpolation_location" = "distance_to_source",
      "inter_segment" = "NAAM"
    )
  ) %>%
  rename(xcoord_inter = xcoord, ycoord_inter = ycoord)

# save the data
write.csv(data, './data/interim/migration.csv')


# filter WS away
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

#add inter_longitude and inter_latitude
#make sf of data
data_sf <- st_as_sf(data_filter, coords = c("xcoord_inter", "ycoord_inter"), crs = 31370) %>%
  st_transform(crs = 4326)
st_coordinates(data_sf)
data_coords <- cbind(data_filter, st_coordinates(data_sf))
data_filter <- data_coords %>%
  rename(inter_longitude = X, inter_latitude = Y)
write.csv(data_filter, './data/interim/migration_filter.csv')
