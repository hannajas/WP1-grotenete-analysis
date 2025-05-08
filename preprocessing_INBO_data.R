library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/calculate_speed_function.R")

# Upload distance matrix
distance_matrix <- read.csv("./data/external/distancematrix_2019_grotenete.csv",  row.names = 1, check.names=FALSE)

# Upload dataset
data <- read_csv('./data/raw/migration.csv')
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# Recalculate the smooth eel track (stop timelimit = 1 hour for original preprocessing)
station_oud <- "random"
# replace in dataframe data in column tag_serial_number "s18a" tot "A"
data$station_name <- gsub("s-8a", "s-8", data$station_name)
data$station_name <- gsub("s-9a", "s-9", data$station_name)
#data$station_name <- gsub("s-10a", "s-10", data$station_name)
data$station_name <- gsub("s-11", "s-12", data$station_name)
for(i in 1:1081){
    #print(paste(i))
    #print(paste(data$station_name[i+1]))
    if((data$station_name[i] == data$station_name[i+1] & data$tag_serial_number[i] == data$tag_serial_number[i+1])){
        station_oud <- data$station_name[i]
        while(data$station_name[i+1] == station_oud){
            data$departure[i] <- data$departure[i+1]
            data$detections[i] <- data$detections[i] + data$detections[i+1]
            data <- data[-(i+1),]
        }
    }
}

# Calculate the alternative speed
residency_list <- split(data , f = data$tag_serial_number)
speed_list <- lapply(residency_list, function(x) movementSpeeds(x, distance_matrix))
speed <- plyr::ldply (speed_list, data.frame)
speed$.id <- NULL

# plot the speed distribution
g <- ggplot()
g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
g <- g + geom_density(aes(x = speed_m_s), data = speed, fill="#69b3a2", color="#e9ecef", alpha=0.8)
g <- g + scale_x_log10(guide = "axis_logticks")
g <- g + geom_vline(xintercept=0.01, linetype = "dotted", linewidth = 1)
print(g)
# ggsave('./figures/speed_m_s_distribution.png')

# calculate wheter the migration was downstream
data_list <- split(speed, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
    x$seg_id <- paste(dplyr::lag(x$station_name),x$station_name, sep = "_")
    x$downstream <- ifelse(x$distance_to_source_m > dplyr::lag(x$distance_to_source_m), TRUE, FALSE)
    cond <- (x$station_name == "s-7"|x$station_name == "s-6"|x$station_name == "s-5"|x$station_name == "s-4c"|x$station_name == "s-4b"|x$station_name == "s-4a")
    x$downstream[(cond)] <- ifelse(x$distance_to_source_m[(cond)] > dplyr::lag(x$distance_to_source_m[(cond)]), FALSE, TRUE)
    x$downstream[x$seg_id=="me-7-2b_s-7"]<- FALSE
    x$downstream[x$seg_id=="s-7_me-7-2b"]<- FALSE
    x$downstream[x$seg_id=="s-7_s-8"]<- TRUE
    x$downstream[x$seg_id=="s-7_s-9"]<- TRUE
    return(x)
})
data <- plyr::ldply(data_temp, data.frame)



# calculate 'downstream_migration'
speed_threshold <- 0.01
data$downstream_migration <- (data$downstream==TRUE & data$speed_m_s >= speed_threshold)# | (data$downstream==TRUE & !(lag(data$migration_speed) >= 30*data$migration_speed)) | (data$downstream==TRUE & !(lead(data$migration_speed) <= 30*data$migration_speed))
data$downstream_migration[data$downstream==TRUE & (lead(data$speed_m_s)*10 <= data$speed_m_s)] <- FALSE
data$downstream_migration <- ifelse((data$downstream==TRUE & (lag(data$migration_speed) >= 30*data$migration_speed)), FALSE, TRUE)

#add column to devide the study area in a tidal, transition and non-tidal area
grenswaardes_distance_to_source <- c(28833.23349, 43106.96)#boundaries between de different zones (looked at receivers distance_to_source and then ruler in QGIS)
data$zone <- "transition"
data$zone[data$distance_to_source_m < grenswaardes_distance_to_source[1]] <- "non-tidal"
data$zone[data$distance_to_source_m > grenswaardes_distance_to_source[2]] <- "tidal"

# save the data
write.csv(data, './data/interim/migration.csv')


# filter WS away
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

write.csv(data_filter, './data/interim/migration_env_filter.csv')