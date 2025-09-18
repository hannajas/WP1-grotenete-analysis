# Comparing the amount of detections in function of the circadian rhythm (similar as in Keirsebelik et al. 2025)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(suncalc)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(patchwork)
library(stats)

# Function to calculate the circadian rhythm
source('./src/circadian_rhythm_function.R')

# Load data
zes00a_1066_Q <- read_csv(
  './data/interim/processed/zes00a_1066_Q.csv',
  show_col_types = FALSE
) #just for the starting and ending time
data_eels <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
filter(
  !tag_serial_number %in%
    c(1171747, 1171751, 1294168, 1294172)
)
circadian <- getSunlightTimes(
  as.Date(zes00a_1066_Q$Timestamp),
  lat = 51.1,
  lon = 5.0,
  keep = c("nightEnd", "sunrise", "sunset", "night")
)

# For each day the duration of each phase
circadian$w_dawn <- as.duration(circadian$sunrise - circadian$nightEnd) /
  (24 * 60 * 60)
circadian$w_day <- as.duration(circadian$sunset - circadian$sunrise) /
  (24 * 60 * 60)
circadian$w_dusk <- as.duration(circadian$night - circadian$sunset) /
  (24 * 60 * 60)
circadian$w_night <- 1 - circadian$w_dusk - circadian$w_day - circadian$w_dawn

# determine for each detection in which circadian phase it falls by making use of circadian_rhythm function
data_eels <- data_eels %>%
  mutate(
    arrival_circadian = unlist(lapply(
      data_eels$arrival,
      circadian_rhythm,
      circadian = circadian
    )),
    departure_circadian = unlist(lapply(
      data_eels$departure,
      circadian_rhythm,
      circadian = circadian
    ))
  )


# calculate $dawn_arr = P("dawn"| arrival = date x) (TODO write shorter)
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
#write.csv(data_eels, './data/interim/migration_circadian.csv', row.names = FALSE)

# plot the arrivals
p1 <- ggplot(data_eels, aes(x = hour(arrival))) +
  geom_bar(aes(fill = arrival_circadian)) +
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  labs(title = "Arrivals at receicers") + #remove legend
  theme(legend.position = "none") +
  facet_wrap(~zone)
# departures
p2 <- ggplot(data_eels, aes(x = hour(departure))) +
  geom_bar(aes(fill = departure_circadian)) +
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  labs(title = "Departures at receicers") + #add a legend to describe the polar axis
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Circadian phase")) +
  facet_wrap(~zone)
print(p1 / p2)
# meeste arrivals en departures tussen 18u en 21u

# compare the proportions of arrivals and departures in the different phases
Proportions_contr <- data_eels %>%
  group_by(zone) %>%
  summarise(
    dawn = mean(dawn_w_arr, na.rm = TRUE),
    day = mean(day_w_arr, na.rm = TRUE),
    dusk = mean(dusk_w_arr, na.rm = TRUE),
    night = mean(night_w_arr, na.rm = TRUE)
  ) %>%
  mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
  pivot_longer(-zone, names_to = "phase", values_to = "proportion") %>%
  pivot_wider(names_from = zone, values_from = proportion)

Counts <- data_eels %>%
  group_by(zone, arrival_circadian) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
  pivot_wider(names_from = zone, values_from = count, values_fill = 0)

#######################################################################
# statistical analysis (chi-squared test)
#######################################################################
#non-tidal
chi_nontidal <- chisq.test(
  x = Counts$non_tidal,
  p = Proportions_contr$non_tidal
)
#tidal
chi_tidal <- chisq.test(
  x = Counts$tidal,
  p = Proportions_contr$tidal
)
#transition
chi_transition <- chisq.test(
  x = Counts$transition,
  p = Proportions_contr$transition
)

#significant difference between dusk and dawn?
keep <- c(1, 3)
chi_nontidal <- chisq.test(
  x = Counts$non_tidal[keep],
  p = Proportions_contr$non_tidal[keep] / sum(Proportions_contr$non_tidal[keep])
)
chi_tidal <- chisq.test(
  x = Counts$tidal[keep],
  p = Proportions_contr$tidal[keep] / sum(Proportions_contr$tidal[keep])
)
chi_transition <- chisq.test(
  x = Counts$transition[keep],
  p = Proportions_contr$transition[keep] /
    sum(Proportions_contr$transition[keep])
) #NOT SIGNIFICANT


##############################################################################################
#visualization
#############################################################################################
Proportions_long <- pivot_longer(
  Proportions,
  cols = c("controlle", "effective"),
  names_to = "type",
  values_to = "proportion"
)

ggplot(Proportions, aes(x = phase, fill = type)) +
  geom_bar(aes(y = proportion), stat = "identity", position = "dodge") +
  labs(title = "Circadian phase proportions") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Circadian phase")) +
  ylab("Proportion") +
  xlab("Circadian phase") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
