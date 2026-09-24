# Merge shad meta data to tracking dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# 1. Source ####
source("./scripts/data_preprocessing/01_attach_release.R")

data$tag_serial_number <- factor(data$tag_serial_number)
data$date_time <- as_datetime(data$date_time)


# 2. Load eel metadata ####
eel <- read.csv("./data/raw/eel_meta_data.csv")
eel$X <- NULL
eel$animal_project_code <- NULL
eel$scientific_name <- NULL
eel$age <- NULL
eel$age_unit <- NULL
eel$treatment_type <- NULL
eel$tag_serial_number <- factor(eel$tag_serial_number)
eel$capture_date_time <- dmy_hm(eel$capture_date_time)
eel$release_date_time <- dmy_hm(eel$release_date_time)

# Return number of tagged eels per year ####
eel$catch_year <- year(eel$capture_date_time)

eel %>%
  group_by(catch_year) %>%
  summarise(tot_eels = n_distinct(tag_serial_number))


# 3. Merge eel characteristics with dataset ####
data <- merge(data, eel, by = "tag_serial_number")


# Return number of detected eels per year ####
data %>%
  group_by(catch_year) %>%
  summarise(tot_eels = n_distinct(tag_serial_number))
