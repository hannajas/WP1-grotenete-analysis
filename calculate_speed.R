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

# save the data
write.csv(speed, './data/interim/migration.csv')