# Extract receiver networks based on detections
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



distinct_stations <- data %>%
  distinct(station_name, .keep_all = TRUE) %>%
  select(animal_project_code, station_name, deploy_latitude, deploy_longitude) %>%
  rename(latitude = deploy_latitude,
         longitude = deploy_longitude)


write.csv(distinct_stations, "./data/interim/receivernetwork_2019_Grotenete.csv", row.names=FALSE)
