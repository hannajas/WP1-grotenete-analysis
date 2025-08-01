library(tidyverse)
library(lubridate)
library(sf)
library(crawl)
library(ggspatial)
library(mapview)
library(prettymapr)
library(dbscan)
library(dplyr)
library(adehabitatLT)
library(factoextra)

data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)

cord.dec <- SpatialPoints(
  data[, c("deploy_longitude", "deploy_latitude")],
  proj4string = CRS("+proj=longlat")
)
test <- spTransform(cord.dec, CRS("+proj=utm +zone=31 +ellps=WGS84"))
data[, c("x", "y")] <- coordinates(test)
resolution_s <- "15 min"
minutes <- 15

data$middledate <- as.POSIXct(data$arrival + data$residence / 2) #PJ werkt op arrival time en niet op middledate
data$rounded_date <- floor_date(data$middledate, unit = resolution_s)
data$tag_serial_number <- as.character(data$tag_serial_number)

id <- unique(data$tag_serial_number)
#Store data in an object of class "ltraj"
xy <- data[, c("x", "y")]
date <- data$middledate
id <- as.character(data$tag_serial_number)
id_unique <- unique(id)
traj <- as.ltraj(xy, data$middledate, id) #traj[[1]]$dist --> 21 object for which the last one is NA
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

  original_times <- data.temp$middledate
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

data_inter_env <- left_join(
  data_inter[, c("x", "y", "date", "dist", "dt", "tag_serial_number")],
  data,
  by = c("tag_serial_number", "date" = "rounded_date")
)

#I want to put values in the data_inter_env$speed_m_s column by filling in all the row above a value with that value

data_inter_env <- data_inter_env %>%
  group_by(tag_serial_number) %>%
  mutate(
    speed_m_s = zoo::na.locf(speed_m_s, fromLast = TRUE, na.rm = FALSE),
    distance_to_source_m = zoo::na.locf(
      distance_to_source_m,
      fromLast = TRUE,
      na.rm = FALSE
    ),
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
    station_name = zoo::na.locf(station_name, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

source("./src/inverse_distance_function.R")
source("./src/align_resolutions_function.R")
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)
env_data_Tw <- align_resolutions_function(
  "Tw",
  as.difftime(5, units = "mins"), #as.period(5, "mins"),
  metadata,
  upsample_method = "ffill",
  data_inter_env
)
p <- 1
test <- inverse_distance(data_inter_env, env_data_Tw, metadata, p, "Tw")


########################################################################

a <- gdistance::shortestPath(transition, A, B, output = "SpatialLines")


# plot the speed of the fish for each segment
pdf("./figures/Downstream_segments_Tw_Q.pdf") # Create pdf
for (i in 2:length(seq_af) - 1) {
  print(paste("Processing segment:", seq_af[i], "to", seq_af[i + 1]))
  lag_station_name <- dplyr::lag(data_filter$station_name)
  print(paste("lag_station_name:", length(lag_station_name)))
  print(paste("station_name:", length(data_filter$station_name)))
  data_temp <- filter(
    data_filter,
    lag_station_name == seq_af[i] & station_name == seq_af[i + 1]
  )
  if (dim(data_temp)[1] == 0) {
    print(paste("No row found with this conditions, i = ", i))
  } else {
    print(paste("data_temp:", dim(data_temp)[1]))
    p1 <- ggplot(data_temp, aes(Tw, speed_m_s)) +
      geom_point(shape = 16, size = 5) +
      geom_smooth(method = lm, size = 2) +
      theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle = 90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25)
      ) +
      labs(x = "Temperature [°C]", y = "speed_m_s")

    p2 <- ggplot(data_temp, aes(Q, speed_m_s)) +
      geom_point(shape = 16, size = 5) +
      geom_smooth(method = lm, size = 2) +
      theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle = 90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25)
      ) +
      labs(x = "Discharge [m^3/s]", y = "speed_m_s")
    #print(p2 / p1 + plot_layout(heights = c(1,2)))
    print(p1)
  }
}
dev.off()


######################################
data_inter_env$station_name == metadata_filter$receiver[1]

find_receiver <- data_inter_env$station_name == metadata_filter$receiver[1]
test <- replace(find_receiver, is.na(find_receiver), FALSE)

test <- replace(
  telemetry_data$temp,
  telemetry_data$station_name == metadata_filter$receiver[i],
  unlist(env_data[
    telemetry_data$station_name == metadata_filter$receiver[i],
    i
  ])
)
