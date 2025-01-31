library(tidyverse)
library(lubridate)
library(tidyquant)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/get_mean_env_var_function.R")

#Loading the raw environmental data
L07_077_Tw <- read_csv('./data/raw/L07_077_Tw.csv')
rup02e_SF_1066_Tw <- read_csv('./data/raw/rup02e_SF_1066_Tw.csv')

#Loading telemetry data
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$arrival <- dmy_hms(data$arrival)
data$departure <- dmy_hms(data$departure)

#Loading the meta-data
metadata_Tw <- read_csv('./data/raw/Metadata_Tw.csv')
n <- dim(metadata_Tw)[1] #number of variables
metadata_Tw$resolution <- as.period(metadata_Tw$resolution_multiplier,metadata_Tw$resolution_unit)
receiver <- lapply(1:n, function(i) {
    data$station_name[which(round(data$distance_to_source_m, digits = 2) == round(metadata_Tw$distance_to_source[i], digits=2))][1]
    })
metadata_Tw$receiver <- unlist(receiver)

#Proces the environmental data
begin1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
L07_077_Tw$Value[begin1:eind1] <- NA
L07_077_Tw$Value[begin2:eind2] <- NA
L07_077_Tw$Value <- as.numeric(L07_077_Tw$Value)
write.csv(L07_077_Tw, './data/interim/processed/L07_077_Tw.csv')
write.csv(rup02e_SF_1066_Tw, './data/interim/processed/rup02e_SF_1066_Tw.csv')


# Allign the environmental data in time to the telemetry data
p <- 1

#data <- mean_env_var(data, metadata_Tw,p)