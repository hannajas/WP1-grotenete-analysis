library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/concat_env_var_function.R")

# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
# need for columns tag_serial_number, migration and station_name to be factors?
L07_077_Tw <- read_csv('./data/raw/L07_077_Tw.csv')

rup02e_SF_1066_Tw <- read_csv('./data/raw/rup02e_SF_1066_Tw.csv')

# META-DATA
# 0 m from release_location
resolution_unit_L <- "minutes"
resolution_unit_rup <- "minutes"
resolution_multiplier_L <- 15
resolution_multiplier_rup <- 5

# unreliable values --> NA (I did a manual screen)
begin1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
L07_077_Tw$Value[begin1:eind1] <- NA
L07_077_Tw$Value[begin2:eind2] <- NA
L07_077_Tw$Value <- as.numeric(L07_077_Tw$Value)

#for 1 value
dep_time <- round_date(data$departure,unit = minutes(15))
arr_time <- round_date(data$arrival,unit = minutes(15))




#data$L07_077_Tw <- mean(L07_077_Tw$Value[L07_077_Tw$Timestamp >= arr_time & L07_077_Tw$Timestamp <= dep_time], na.rm=TRUE)
#data$L07_077_Tw <- rowMeans(L07_077_Tw$Value[L07_077_Tw$Timestamp >= arr_time & L07_077_Tw$Timestamp <= dep_time], na.rm=TRUE)

# MAKE USE OF LAPPLY to avoid for loops
#x <- L070_077_Tw
f1 <- function(i,x)  mean(x$Value[x$Timestamp >= arr_time[i] & x$Timestamp <= dep_time[i]], na.rm=TRUE)
data$L07_077_Tw <- lapply(1:length(dep_time), f1, x = L07_077_Tw)
#mean(L07_077_Tw$Value[L07_077_Tw$Timestamp >= test1 & L07_077_Tw$Timestamp <= test2], na.rm=TRUE)
