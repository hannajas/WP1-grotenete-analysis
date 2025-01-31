library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/inverse_distance_function.R")

# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# need for columns tag_serial_number, migration and station_name to be factors?
L07_077_Tw <- read_csv('./data/raw/L07_077_Tw.csv')
rup02e_SF_1066_Tw <- read_csv('./data/raw/rup02e_SF_1066_Tw.csv')

# META-DATA
# 0 m from release_location
metadata_Tw <- read_csv('./data/raw/Metadata_Tw.csv')
n <- dim(metadata_Tw)[1] #number of variables
metadata_Tw$resolution <- as.period(metadata_Tw$resolution_multiplier,metadata_Tw$resolution_unit)
receiver <- lapply(1:n, function(i) {
    data$station_name[which(round(data$distance_to_source_m, digits = 2) == round(metadata_Tw$distance_to_source[i], digits=2))][1]
    })
metadata_Tw$receiver <- unlist(receiver)

# PRE_PROCESSING
# unreliable values --> NA (I did a manual screen)
begin1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
L07_077_Tw$Value[begin1:eind1] <- NA
L07_077_Tw$Value[begin2:eind2] <- NA
L07_077_Tw$Value <- as.numeric(L07_077_Tw$Value)
L07_077_Tw$Timestamp <- ymd_hms(L07_077_Tw$Timestamp)
rup02e_SF_1066_Tw$Timestamp <- ymd_hms(rup02e_SF_1066_Tw$Timestamp)

# unrealistic speed values
data <- filter(data, speed_m_s <= 5)
#could be shorter for when their are lots of environmental variables

data$L07_077 <- concat_env_var(data, metadata_Tw[1,])
data$rup02e_SF_1066 <- concat_env_var(data, metadata_Tw[2,])
env_data <- data[(dim(data)[2]-(n-1)):dim(data)[2]]

# INVERSE DISTANCE WEIGHTING
# use idw function from spatstat explore (ppp object is input)
# from dim(data)[2] to dim(data[2])-n
p <- 1
data$Tw <- inverse_distance(data, env_data, metadata_Tw,p)

d1 <- ggplot()
d1 <- d1 + geom_point(aes(Tw, speed_m_s), data = data, shape = 16, size = 5)
d1