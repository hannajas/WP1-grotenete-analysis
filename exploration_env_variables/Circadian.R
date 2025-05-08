library(suncalc)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(patchwork)

# Function to calculate the circadian rhythm
zes00a_1066_Q <- read_csv('./data/interim/processed/zes00a_1066_Q.csv', show_col_types = FALSE)
data_eels <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
circadian <- getSunlightTimes(as.Date(zes00a_1066_Q$Timestamp),lat = 51.1, lon = 5.0, keep=c("nightEnd","sunrise","sunset", "night"))


# add columns to data_eels (arrival_circadian, departure_circadian) by making use of circadian_rhythm function
data_eels <- data_eels %>%
  mutate(arrival_circadian = unlist(lapply(data_eels$arrival, circadian_rhythm, circadian = circadian)),
         departure_circadian = unlist(lapply(data_eels$departure, circadian_rhythm, circadian = circadian)))


# calculate $dawn_arr = P("dawn"| arrival = date x)
data_eels$dawn_w_arr <- unlist(lapply(data_eels$arrival, function(x) {
  circadian$w_dawn[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$dawn_w_dep <- unlist(lapply(data_eels$departure, function(x) {
  circadian$w_dawn[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$day_w_dep <- unlist(lapply(data_eels$departure, function(x) {
  circadian$w_day[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$day_w_arr <- unlist(lapply(data_eels$arrival, function(x) {
  circadian$w_day[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$dusk_w_arr <- unlist(lapply(data_eels$arrival, function(x) {
  circadian$w_dusk[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$dusk_w_dep <- unlist(lapply(data_eels$departure, function(x) {
  circadian$w_dusk[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$night_w_arr <- unlist(lapply(data_eels$arrival, function(x) {
  circadian$w_night[which(circadian$date == floor_date(x, unit = "day"))]
}))
data_eels$night_w_dep <- unlist(lapply(data_eels$departure, function(x) {
  circadian$w_night[which(circadian$date == floor_date(x, unit = "day"))]
}))


# plot the arrivals 
p1 <- ggplot(data_eels, aes(x=hour(arrival)))+
    geom_bar(aes(fill=arrival_circadian))+
    coord_radial(r.axis.inside=TRUE, expand = FALSE)+
    labs(title = "Arrivals at receicers")+#remove legend
    theme(legend.position = "none")+
    facet_wrap(~zone)
# departures
p2 <- ggplot(data_eels, aes(x=hour(departure)))+
    geom_bar(aes(fill=departure_circadian))+
    coord_radial(r.axis.inside=TRUE, expand = FALSE)+
    labs(title = "Departures at receicers")+#add a legend to describe the polar axis
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(title="Circadian phase"))+
    facet_wrap(~zone)
print(p1 / p2)
# meeste arrivals en departures tussen 18u en 21u

#non-tidal arrivals


#weight the counts on the duration of each phase
circadian$w_dawn <- as.duration(circadian$sunrise - circadian$nightEnd)/(24*60*60)
circadian$w_day <- as.duration(circadian$sunset - circadian$sunrise)/(24*60*60)
circadian$w_dusk <- as.duration(circadian$night - circadian$sunset)/(24*60*60)
circadian$w_night <- 1 - circadian$w_dusk - circadian$w_day - circadian$w_dawn

# compare the proportions of arrivals and departures in the different phases
Proportions <- data.frame(phase=c("dawn","day","dusk","night"))

Proportions$controlle <- c(mean(data_eels$dawn_w_arr, na.rm = TRUE),
         mean(data_eels$day_w_arr, na.rm = TRUE),
         mean(data_eels$dusk_w_arr, na.rm = TRUE),
         mean(data_eels$night_w_arr, na.rm = TRUE))

Proportions$effective <- c(mean(data_eels$arrival_circadian == "dawn", na.rm = TRUE),
         mean(data_eels$arrival_circadian == "day", na.rm = TRUE),
         mean(data_eels$arrival_circadian == "dusk", na.rm = TRUE),
         mean(data_eels$arrival_circadian == "night", na.rm = TRUE))
Proportions <- pivot_longer(Proportions, cols = c("controlle", "effective"), names_to = "type", values_to = "proportion")

ggplot(Proportions, aes(x=phase, fill = type)) +
  geom_bar(aes(y=proportion), stat="identity", position= "dodge") +
  labs(title = "Circadian phase proportions") +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Circadian phase")) +
  ylab("Proportion") +
  xlab("Circadian phase") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))