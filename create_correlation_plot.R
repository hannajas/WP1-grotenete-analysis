library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)


# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$tag_serial_number <- factor(data$tag_serial_number)
data$migration <- factor(data$migration)
data$station_name <- factor(data$station_name)

env_data <- read_csv('./data/raw/photoperiod_verwerkt.csv')
env_data$photoperiod <- as.numeric(hms(env_data$photoperiod),"minutes")
env_data$delta_photoperiod <- env_data$photoperiod - dplyr::lag(env_data$photoperiod)
env_data$date <- ymd(env_data$date,tz="UTC")
mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs
env_migration <- data.frame()

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")
    mydfjoin.temp <- left_join(mydfnew.temp, env_data, by = "date")
    mydfnew.split.eel[[i]] <- mydfjoin.temp
    env_migration <- rbind(env_migration, mydfnew.split.eel[[i]])
}

# plot photoperiod
g <- ggplot()
g <- g + geom_point(aes(photoperiod, migration), data = env_migration, shape = 16, size = 5)

g1 <- ggplot()
g1 <- g1 + geom_point(aes(photoperiod, speed_m_s), data = env_migration, shape = 16, size = 5)
print(g / g1)
ggsave("./figures/correlation_photoperiod.png")

# plot delta_photoperiod
d <- ggplot()
d <- d + geom_point(aes(delta_photoperiod, migration), data = env_migration, shape = 16, size = 5)

d1 <- ggplot()
d1 <- d1 + geom_point(aes(delta_photoperiod, speed_m_s), data = env_migration, shape = 16, size = 5)
d1 <- d1 + scale_y_log10(guide = "axis_logticks")
print(d / d1)
ggsave("./figures/correlation_delta_photoperiod.png")