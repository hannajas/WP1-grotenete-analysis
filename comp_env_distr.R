library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#get all the file names in folder "./data/interim/processed/"
data_eels <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
mydfnew.split.eel <- split(data_eels, data_eels$tag_serial_number) # split dataset based on tag IDs

metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_Tw <- filter(metadata, metadata$type == "Tw") 

files <- list.files(path = "./data/interim/processed/", pattern = "*.csv", full.names = TRUE)
path <- files[grepl("Tw", files, fixed=TRUE)]
distribution <- data.frame()
for (k in 1:length(mydfnew.split.eel)){
    start <- min(mydfnew.split.eel[[k]]$arrival)
    end <- max(mydfnew.split.eel[[k]]$departure)
     #print k
     print(paste(k))
    for (i in 1:length(path)) {
        temp <- read_csv(path[i], show_col_types = FALSE)
        temp <- temp %>% filter(Timestamp >= start & Timestamp <= end)
        #concat to previous temp data.frame
        distribution <- rbind(distribution, temp)
    }
}#TO DO remove "...2" column out of zes24a_SF_1066_Tw.csv