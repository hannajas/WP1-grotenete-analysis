library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)


# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
#data$tag_serial_number <- factor(data$tag_serial_number)
#data$migration <- factor(data$migration)
#data$station_name <- factor(data$station_name)

env_data <- read_csv('./data/raw/photoperiod_verwerkt.csv')
env_data$photoperiod <- as.period(hms(env_data$photoperiod),unit="minutes")
env_data$photoperiod <- as.numeric(env_data$photoperiod,"minutes")
resolution <- as.period(1,"days")

env_data$date <- ymd_hms(env_data$date,truncated = 3)#ydm for temperature
dep_time <- round_date(data$departure,unit = resolution)
arr_time <- round_date(data$arrival,unit = resolution)

# calculate the mean temperature between arrival an departure
temp <- unlist(lapply(1:length(dep_time), function(i,x) { 
    mean(x$photoperiod[x$date >= dep_time[i-1] & x$date <= arr_time[i]], na.rm=TRUE) }, x = env_data))
data$photoperiod <- temp
data$delta_photoperiod <- data$photoperiod - dplyr::lag(data$photoperiod)

data_filter <- filter(data, !startsWith(data$station_name, "ws-"))

d1 <- ggplot()
d1 <- d1 + geom_point(aes(photoperiod, speed_m_s), data = data_filter, shape = 16, size = 5)
d1 <- d1 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d1 <- d1 +   labs(x = "photoperiod [min]",y = "speed_m_s")
d1 <- d1 + xlim(460,700)


d2 <- ggplot()
d2 <- d2 + geom_point(aes(downstream_migration, photoperiod), data = data_filter, shape = 16, size = 5)
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d2 <- d2 +   labs(x = "photoperiod [min]",y = "migration")
# ggsave('./figures/photoperiod_migration.png')

d2 <- ggplot()
d2 <- d2 + geom_point(aes(delta_photoperiod, speed_m_s), data = data_filter, shape = 16, size = 5)
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d2 <- d2 + xlim(-5,5)

d2 <- d2 +   labs(x = "photoperiod [min]",y = "migration")

















env_data$delta_photoperiod <- lead(env_data$photoperiod) - env_data$photoperiod
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