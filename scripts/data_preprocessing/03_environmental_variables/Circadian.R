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
  './data/interim/processed/zes00a_1066_Q.csv', #just for timerange
  show_col_types = FALSE
)

data_eels <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  filter(!tag_serial_number %in% c(1171747, 1171751, 1294168, 1294172)) %>% #four eels with only one detection
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
  filter(label %in% c("migratory", "resting")) %>%
  filter(!startsWith(station_name, "rel"))

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
#3 phases? twilight_log = TRUE --> dawn + dusk
twilight_log <- TRUE

# Function to compute circadian weights ----------------------------------
compute_circadian_weights <- function(circadian, phase_defs) {
  #duration proportions of each phase
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
# write.csv(data_eels, './data/interim/migration_circadian.csv', row.names = FALSE)
# data_eels <- read_csv('./data/interim/migration_circadian.csv') %>%
#   filter(!startsWith(station_name, "rel"))

# plot the arrivals
colorscheme <- c(
  day = grey2,
  night = grey3,
  twilight = grey1
)
# departures (with legend)
p2 <- ggplot(data_eels, aes(x = hour(departure) + 0.5)) +
  geom_bar(aes(fill = departure_circadian), width = 0.85) +
  scale_fill_manual(values = colorscheme, na.value = "grey60") +
  coord_radial(r.axis.inside = FALSE, expand = FALSE) + #start = pi/2
  labs(
    fill = "Circadian phase:",
    x = "Hour of departure",
    y = "# observations"
  ) +
  style +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 23, by = 4),
    expand = c(0, 0)
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 25),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 28),
    # axis.title.x = element_text(size = 24),
    # axis.title.y = element_text(size = 24),
    # axis.text.x = element_text(size = 24),
    # axis.text.y = element_text(size = 24)
    axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 28),
    axis.title.y = element_text(hjust = 0.87, margin = margin(r = 8))
  ) + #rename legend title
  facet_wrap(~zone)

#print(p1 / p2)
print(p2)
ggsave(
  "./figures/Circadian/circadian_tidal_dep_bw_new.png",
  width = 12,
  height = 6
)
# meeste arrivals en departures tussen 18u en 21u

# compare the proportions of arrivals and departures in the different phases
Proportions_contr <- data_eels %>%
  group_by(zone) %>%
  summarise(
    dawn = mean(dawn_w_arr, na.rm = TRUE),
    day = mean(day_w_arr, na.rm = TRUE),
    dusk = mean(dusk_w_arr, na.rm = TRUE),
    night = mean(night_w_arr, na.rm = TRUE),
    twilight = mean(twilight_w_arr, na.rm = TRUE)
  ) %>%
  mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
  pivot_longer(-zone, names_to = "phase", values_to = "proportion") %>%
  pivot_wider(names_from = zone, values_from = proportion)

if (twilight_log) {
  Proportions_contr <- Proportions_contr %>%
    filter(phase != "dawn" & phase != "dusk")
} else {
  Proportions_contr <- Proportions_contr %>%
    filter(phase != "twilight")
}

Counts <- data_eels %>%
  group_by(zone, arrival_circadian) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
  pivot_wider(names_from = zone, values_from = count, values_fill = 0)


Counts <- data_eels %>%
  group_by(zone, departure_circadian) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(zone = recode(zone, `non-tidal` = "non_tidal")) %>%
  pivot_wider(names_from = zone, values_from = count, values_fill = 0)

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

#######################################################################
# percentage between 18 and 21u
#######################################################################
read_csv('./data/interim/migration_circadian.csv') -> data_eels
data_eels %>%
  filter(
    hour(arrival) >= 17 & hour(arrival) <= 22
  ) %>%
  group_by(zone) %>%
  summarise(n = n()) %>%
  left_join(
    data_eels %>%
      group_by(zone) %>%
      summarise(total = n()),
    by = "zone"
  ) %>%
  mutate(percentage = n / total * 100)

5 / 24

#######################################################################
#argumentation to use only (arrivals or departures)
data
test <- data_eels[data_eels$arrival + minutes(15) >= data_eels$departure, ] %>%
  nrow()

perc_equal <- test / nrow(data_eels) * 100
