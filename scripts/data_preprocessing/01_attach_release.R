# Add eel release positions and date-time to detection dataset
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# 1. Read eel meta data
eel <- read_csv("./data/raw/eel_meta_data.csv")

eel <- select(eel, 
              animal_project_code, 
              release_date_time, 
              tag_serial_number, 
              release_location, 
              release_latitude, 
              release_longitude)


eel$release_location <- factor(eel$release_location)


# 2. Read file with release location and station
release <- read_csv("./data/external/release_locations_stations.csv")

release$release_location <- factor(release$release_location)
release$release_station <- factor(release$release_station)

# 3. Merge release station with eel data
eel <- left_join(eel, release, by = "release_location")

# 4. Process eel dataset column names
eel$receiver_id <- "none"
eel$acoustic_project_code <- "2019_Grotenete"

eel <- select(eel,
              animal_project_code,
              acoustic_project_code,
              release_date_time, 
              tag_serial_number, 
              release_station,
              receiver_id,
              release_latitude, 
              release_longitude)

eel <- rename(eel,
              date_time = release_date_time,
              station_name = release_station,
              deploy_latitude = release_latitude,
              deploy_longitude = release_longitude)

eel$date_time <- dmy_hm(eel$date_time)

# 5. Merge eel releases to the detection dataset
data <- read_csv("./data/raw/raw_detection_data.csv")
data$...1 <- NULL
data$acoustic_tag_id <- NULL
data <- rbind(data, eel)








