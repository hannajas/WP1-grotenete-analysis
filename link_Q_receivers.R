library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/concat_env_var_function_Q.R")
source("./src/inverse_distance_function.R")

# Upload dataset
data <- read_csv('./data/interim/migration.csv')
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# META-DATA
# 0 m from release_location
metadata_Tw <- read_csv('./data/raw/metadata/Metadata_Q.csv')
n <- dim(metadata_Tw)[1] #number of variables
test <- as.period(metadata_Tw$resolution_multiplier,metadata_Tw$resolution_unit[1])
test[4] <- period(metadata_Tw$resolution_multiplier[4],metadata_Tw$resolution_unit[4])
metadata_Tw$resolution <- test
receiver <- lapply(1:n, function(i) {
    data$station_name[which(round(data$distance_to_source_m, digits = 2) == round(metadata_Tw$distance_to_source[i], digits=2))][1]
    })
metadata_Tw$receiver <- unlist(receiver)

# Load the environmental data
for (i in 1:n) {
    path <- paste('./data/raw/discharge/',metadata_Tw$name[i],'_Q.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_Tw$name[i],'_Q', sep =""), temp)
}

# PRE_PROCESSING
rup00a_1066_Q$Timestamp <- floor_date(dmy_hms(rup00a_1066_Q$Timestamp,truncated=3),"day")
for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_Tw$name[i],'_Q.csv', sep ="")
    write.csv(get(paste(metadata_Tw$name[i],'_Q', sep ="")), path)
}


# middle out variable to fit the telemetry data
for (i in 1:n) {
    data[metadata_Tw$name[i]] <- concat_env_var(data, metadata_Tw[i,])
}
env_data <- data[(dim(data)[2]-(n-1)):dim(data)[2]]

# INVERSE DISTANCE WEIGHTING
# use idw function from spatstat explore (ppp object is input)
# from dim(data)[2] to dim(data[2])-n
p <- 1
data$Tw <- inverse_distance(data, env_data, metadata_Tw,p)
data$delta_Tw <- dplyr::lag(data$Tw) - data$Tw

# unrealistic speed values
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

#plot correlation
d1 <- ggplot()
d1 <- d1 + geom_point(aes(Tw, speed_m_s), data = data_filter, shape = 16, size = 5)
d1
p <- ggplot(data_filter, aes(Tw, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Debiet [m^3/s]",
    y = "speed_m_s")
#ggsave('./figures/correlations/debiet.png')



p1 <- ggplot(data_filter, aes(Tw, downstream_migration))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Debiet [m^3/s]",
    y = "migration")