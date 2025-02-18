library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/calculate_speed_function.R")

distance_matrix <- read.csv("./data/external/distancematrix_2019_grotenete.csv",  row.names = 1, check.names=FALSE)

# Upload dataset
data <- read_csv('./data/raw/migration.csv')
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# Turn dataset into list per tag_serial_number
residency_list <- split(data , f = data$tag_serial_number)
#sapply(residency_list, function(x) max(x$detections))

# Calculate speed per tag_serial_number
speed_list <- lapply(residency_list, function(x) movementSpeeds(x, distance_matrix))
#speed_list[[1]]

# Turn lists back into dataframe
#speed <- do.call(rbind.data.frame, speed)
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL



#data <- movementSpeeds(speed, distance_matrix)
write.csv(speed, './data/interim/migration.csv')