library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)
library(geosphere)

# Source functions
source("./src/concat_all_env_var_functions.R")

# load data
data <- read_csv('./data/interim/migration.csv', show_col_types = FALSE)
metadata_eel <- read_csv('./data/raw/eel_meta_data.csv', show_col_types = FALSE)
metadata <- read_csv('./data/interim/Metadata.csv', show_col_types = FALSE)
data <- filter(data, (!startsWith(data$station_name, "ws-") & data$downstream == TRUE))

##################################################################################################################
# Environmental variables

#boxplot of speed for each seg_id
data %>%
  ggplot(aes(x = reorder(seg_id, distance_to_source_m, FUN = mean, na.rm = TRUE), y = migration_speed)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Average speed of the eel along its whole migration route
data_list <- split(data, f = data$tag_serial_number)
average_speed <- lapply(data_list, function(x) {
    swim_distance <- tail(x$swimdistance_m, n = 1)
    swim_time <- as.vector(difftime(tail(x$departure,n=1),
      x$arrival[1], units = "secs"))
    average_speed_temp <- swim_distance/swim_time
    return(average_speed_temp)
})
average_speed <- plyr::ldply(average_speed)
average_speed$tag_serial_number <-
  as.numeric(average_speed$.id)
average_speed$.id <- NULL
eeldata <- left_join(metadata_eel, average_speed, by = "tag_serial_number")

eeldata %>%
  ggplot(aes(x = length1+weight, y = V1)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# densitiy plot of the average speed
data %>%
  group_by(tag_serial_number) %>%
  summarise(mean_speed = mean(migration_speed, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_speed)) +
  geom_density() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# distance on passing time for all eels