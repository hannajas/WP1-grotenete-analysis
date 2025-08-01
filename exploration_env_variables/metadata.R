library(wateRinfo)
library(tidyverse)
library(rvest)

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
receiver <- lapply(1:n_data, function(i) {
  data$station_name[which(
    round(data$distance_to_source_m, digits = 2) ==
      round(metadata$distance_to_source[i], digits = 2)
  )][1]
})
metadata$receiver <- unlist(receiver)


write.csv(metadata, './data/interim/metadata.csv')
