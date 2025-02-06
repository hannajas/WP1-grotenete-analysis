library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/inverse_distance_function.R")

# Upload dataset
data <- read_csv('./data/raw/migration.csv')
data$...1 <- NULL

# META-DATA
# 0 m from release_location
metadata_Tw <- read_csv('./data/raw/metadata/Metadata_Tw.csv')
n <- dim(metadata_Tw)[1] #number of variables
metadata_Tw$resolution <- as.period(metadata_Tw$resolution_multiplier,metadata_Tw$resolution_unit)
receiver <- lapply(1:n, function(i) {
    data$station_name[which(round(data$distance_to_source_m, digits = 2) == round(metadata_Tw$distance_to_source[i], digits=2))][1]
    })
metadata_Tw$receiver <- unlist(receiver)

# need for columns tag_serial_number, migration and station_name to be factors?
for (i in 1:n) {
    path <- paste('./data/raw/temperature/',metadata_Tw$name[i],'_Tw.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_Tw$name[i],'_Tw', sep =""), temp)
}

# PRE_PROCESSING
# unreliable values --> NA (I did a manual screen)
begin1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
L07_077_Tw$Value[begin1:eind1] <- NA
L07_077_Tw$Value[begin2:eind2] <- NA
L07_077_Tw$Value <- as.numeric(L07_077_Tw$Value)
#L07_077_Tw$Timestamp <- ymd_hms(L07_077_Tw$Timestamp)
#rup02e_SF_1066_Tw$Timestamp <- ymd_hms(rup02e_SF_1066_Tw$Timestamp)

for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_Tw$name[i],'_Tw.csv', sep ="")
    write.csv(get(paste(metadata_Tw$name[i],'_Tw', sep ="")), path)
}


#could be shorter for when their are lots of environmental variables
for (i in 1:n) {
    data[metadata_Tw$name[i]] <- concat_env_var(data, metadata_Tw[i,])
}
env_data <- data[(dim(data)[2]-(n-1)):dim(data)[2]]

# INVERSE DISTANCE WEIGHTING
# use idw function from spatstat explore (ppp object is input)
# from dim(data)[2] to dim(data[2])-n
p <- 1
data$Tw <- inverse_distance(data, env_data, metadata_Tw,p)
data$delta_Tw <- data$Tw - dplyr::lag(data$Tw)

# unrealistic speed values
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

#plot correlation
d1 <- ggplot()
d1 <- d1 + geom_point(aes(Tw, speed_m_s), data = data_filter, shape = 16, size = 5)
d1 <- d1 + geom_smooth(method=lm)
d1 <- d1 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
#d1 <- d1 + xlim(-6,3)
ggsave('./figures/correlations/delta_watertemperature.png')

p <- ggplot(data_filter, aes(Tw, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))
ggsave('./figures/correlations/watertemperature.png')

g <- ggplot(data_filter, aes(Tw, downstream_migration))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))
ggsave('./figures/watertemperature_mirgation.png')


#plot correlation
d1 <- ggplot()
d1 <- d1 + geom_point(aes(delta_Tw, speed_m_s), data = data_filter_Tw, shape = 16, size = 5)
d1 <- d1 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d1 <- d1 + xlim(-6,3)