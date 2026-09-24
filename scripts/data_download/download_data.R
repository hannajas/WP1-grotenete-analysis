# Download eel tracking data from River Grote Nete (2019_Grotenete) from ETN database via RStudio LifeWatch server
# by Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

# Install and load packages
install.packages("devtools")
install.packages("digest")
install.packages("remotes")
remotes::install_github("inbo/etn@v2.3-beta")

library(dplyr)
library(etn)


# 1. Download detection data ####
data <- get_acoustic_detections(
  my_con,
  scientific_name = "Anguilla anguilla",
  animal_project_code = "2019_Grotenete",
  limit = FALSE
)

# Select relevant columns
data <- select(
  data,
  animal_project_code,
  acoustic_project_code,
  date_time,
  tag_serial_number,
  acoustic_tag_id,
  station_name,
  receiver_id,
  deploy_latitude,
  deploy_longitude
)


# 2. Download eel meta-data ####
eels <- get_animals(
  my_con,
  scientific_name = "Anguilla anguilla",
  animal_project_code = "2019_Grotenete"
)

# Select relevant columns
eels <- select(
  eels,
  animal_project_code,
  scientific_name,
  tag_serial_number,
  acoustic_tag_id,
  capture_date_time,
  capture_location,
  capture_latitude,
  capture_longitude,
  capture_method,
  release_date_time,
  release_location,
  release_latitude,
  release_longitude,
  length1_type,
  length1,
  length1_unit,
  length2_type,
  length2,
  length2_unit,
  length3_type,
  length3,
  length3_unit,
  length4_type,
  length4,
  length4_unit,
  weight,
  weight_unit,
  age,
  age_unit,
  sex,
  life_stage,
  treatment_type
)


# 3. Download deployment positions ####
network_projects <- c(
  "lifewatch",
  "2013_Maas",
  "dijle",
  "albert",
  "bpns",
  "ws1",
  "ws2",
  "ws3",
  "zeeschelde",
  "2019_Grotenete",
  "life4fish"
)


deployments <- get_acoustic_deployments(
  my_con,
  acoustic_project_code = network_projects,
  open_only = FALSE
)
deployments$station_name <- factor(deployments$station_name)

# Get unique deployments
unique_deployments <- deployments %>%
  group_by(acoustic_project_code) %>%
  distinct(station_name, .keep_all = TRUE)

# Select relevant columns
unique_deployments <- select(
  unique_deployments,
  acoustic_project_code,
  station_name,
  deploy_latitude,
  deploy_longitude
)


write.csv(data, "raw_detection_data.csv")
write.csv(eels, "eel_meta_data.csv")
write.csv(unique_deployments, "deployments.csv")
