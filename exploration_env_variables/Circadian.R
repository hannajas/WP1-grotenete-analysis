# Comparing the amount of detections in function of the circadian rhythm
# (similar as in Keirsebelik et al. 2025)
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
library(purrr)

# Function to calculate the circadian rhythm
source('./src/circadian_rhythm_function.R')

# Load data --------------------------------------------------------------
zes00a_1066_Q <- read_csv(
  './data/interim/processed/zes00a_1066_Q.csv',
  show_col_types = FALSE
)

data_eels <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  filter(!tag_serial_number %in% c(1171747, 1171751, 1294168, 1294172)) %>%
  mutate(
    label = ifelse(
      cluster == 1,
      "resident/resting",
      ifelse(cluster == 2, "migratory", NA)
    )
  ) %>%
  mutate(
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE),
    year = year(arrival[1])
  ) %>%
  mutate(
    label = case_when(
      label == "resident/resting" & row_number() < first_migratory_idx ~
        "resident",
      label == "resident/resting" & row_number() > first_migratory_idx ~
        "resting",
      label == "migratory" ~ "migratory",
      is.infinite(first_migratory_idx) | is.na(first_migratory_idx) ~ "resident"
    )
  ) %>%
  ungroup() %>%
  filter(label %in% c("migratory", "resting"))

# Sunlight phases --------------------------------------------------------
circadian <- getSunlightTimes(
  as.Date(zes00a_1066_Q$Timestamp),
  lat = 51.216667, # same location as daylength
  lon = 4.6,
  keep = c("nightEnd", "sunrise", "sunset", "night")
)

# --- CONFIG: choose phases here ---
#night should be the remainder phase!
# Option A: 4 phases
phase_defs <- list(
  dawn = c("nightEnd", "sunrise"),
  day = c("sunrise", "sunset"),
  dusk = c("sunset", "night")
)
#3 phases? trwilight_log = TRUE --> dawn + dusk
twilight_log <- TRUE

# Function to compute circadian weights ----------------------------------
compute_circadian_weights <- function(circadian, phase_defs) {
  weights <- map(
    phase_defs,
    ~ as.duration(circadian[[.[2]]] - circadian[[.[1]]]) / ddays(1)
  )
  circadian[paste0("w_", names(phase_defs))] <- weights
  circadian$w_night <- 1 - rowSums(circadian[paste0("w_", names(phase_defs))])
  circadian
}

circadian <- compute_circadian_weights(circadian, phase_defs)

# Derived phase if wanted (twilight = dawn + dusk in 4-phase system)
if (all(c("w_dawn", "w_dusk") %in% names(circadian))) {
  circadian$w_twilight <- circadian$w_dawn + circadian$w_dusk
}

# Annotate detections with circadian phase -------------------------------
data_eels <- data_eels %>%
  mutate(
    arrival_circadian = map_chr(
      arrival,
      circadian_rhythm,
      circadian = circadian,
      twilight = twilight_log
    ),
    departure_circadian = map_chr(
      departure,
      circadian_rhythm,
      circadian = circadian,
      twilight = twilight_log
    )
  )

# Calculate probabilities per event & phase ------------------------------
periods <- gsub("^w_", "", grep("^w_", names(circadian), value = TRUE))
events <- c("arrival", "departure")

grid <- expand.grid(period = periods, event = events, stringsAsFactors = FALSE)

walk2(grid$period, grid$event, function(p, e) {
  col_name <- paste0(p, "_w_", substr(e, 1, 3)) # e.g. "dawn_w_arr"
  data_eels[[col_name]] <<- map_dbl(data_eels[[e]], function(x) {
    circadian[[paste0("w_", p)]][circadian$date == floor_date(x, "day")]
  })
})
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
#ggsave("./figures/Circadian/circadian_tidal.png")
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
twilight <- c(
  "twilight",
  sum(Counts$non_tidal[c(1, 3)]),
  sum(Counts$tidal[c(1, 3)]),
  sum(Counts$transition[c(1, 3)])
)

# Counts <- data_eels %>%
#   group_by(zone, departure_circadian) %>%
#   summarise(count = n(), .groups = 'drop') %>%
#   mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
#   pivot_wider(names_from = zone, values_from = count, values_fill = 0)

#######################################################################
# statistical analysis (chi-squared test)
#######################################################################
#non-tidal
#H_0: non-selective migration (based on night/day)
#assumptions: EXPECTED observations > 5
chi_nontidal <- chisq.test(
  x = Counts$non_tidal,
  p = Proportions_contr$non_tidal #relative duration of the phases
)
#chi-square test with 3 degrees of freedom (4 phases - 1 = 3)
#H_0: 100% noctural migration --> voilation of assumptions (expected < 5)

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
