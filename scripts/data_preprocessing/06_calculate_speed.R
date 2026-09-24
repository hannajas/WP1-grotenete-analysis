# Calculate migration speeds between consecutive detection stations
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(actel)
#browseVignettes("actel")

# Source functions
source("./src/calculate_speed_function.R")
source("./src/calculate_sourcedistance_function.R")

# Load residency data
residency <- read_csv("./data/interim/residency.csv")
residency$...1 <- NULL


# Load distance matrix
# Make sure the first column is not containing the station names
distance_matrix <- read.csv("./data/external/distancematrix_2019_grotenete.csv",  row.names = 1, check.names=FALSE)

# Calculate speed without taking into account different tag_serial_number
#speed <- movementSpeeds(residency, "last to first", distance_matrix)

# Turn dataset into list per tag_serial_number
residency_list <- split(residency , f = residency$tag_serial_number)
#sapply(residency_list, function(x) max(x$detections))

# Calculate speed per tag_serial_number
speed_list <- lapply(residency_list, function(x) movementSpeeds(x, "last to first", distance_matrix))
#speed_list[[1]]

# Turn lists back into dataframe
#speed <- do.call(rbind.data.frame, speed)
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL


# Calculate total swim distance per tag_id
speed <- speed %>% 
  group_by(tag_serial_number) %>%
  mutate(totaldistance_m=cumsum(coalesce(swimdistance_m, 0)) + swimdistance_m*0)

# Set row with release station at 0 total distance
speed <- speed %>% 
  #group_by(id) %>% 
  mutate(totaldistance_m = ifelse(row_number() == 1 | all(is.na(totaldistance_m)), 0, totaldistance_m))

# Replace the remaining NA values with previous non-NA value
speed$totaldistance_m <- zoo::na.locf(speed$totaldistance_m)

# Calculate the the station distance from a 'source' station
speed <- distanceSource(speed, "last to first", distance_matrix)

# Set 'NA' to '0'
speed <- speed %>% 
  replace_na(list(distance_to_source_m = 0))
                     
                     

# Write csv
write.csv(speed, "./data/interim/speed.csv")


