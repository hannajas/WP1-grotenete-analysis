# Comparing the amount of detections in function of the lunar rhythm
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(wateRinfo)
library(tidyr)
library(tidyverse)
library(patchwork)
library(fuzzyjoin)
library(lubridate)

lunar_cycle <- read_csv(
  "./data/raw/lunar_cycle/lunar_cycle.csv",
  show_col_types = FALSE
)

# read (meta)data and tide data
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

metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_tij <- filter(metadata, metadata$type == "tij")
n <- dim(metadata_tij)[1]


###########################################################################################
# link lunar cycle data to detection data
###########################################################################################
lunar_cycle <- lunar_cycle %>%
  rename(Time = `Time (Universal Time)`) %>%
  mutate(
    # combine Date and Time into a single POSIXct datetime (UTC)
    date = lubridate::parse_date_time(
      paste(Date, Time),
      orders = c("ymd HMS", "ymd HM", "ymd"),
      tz = "UTC"
    )
  ) %>%
  select(-c(Time, Date))

# ensure there is a standardized moon_phase column
if ("Moon Phase" %in% names(lunar_cycle)) {
  lunar_cycle <- lunar_cycle %>% rename(moon_phase = `Moon Phase`)
}

# find quarter events and build consecutive intervals
quarters <- lunar_cycle %>%
  mutate(phase_lower = tolower(moon_phase)) %>%
  filter(str_detect(
    phase_lower,
    "first quarter|first_quarter|first|last quarter|last_quarter|last|third quarter|third_quarter"
  )) %>%
  mutate(
    phase_short = case_when(
      str_detect(phase_lower, "first quarter|first_quarter|first") ~ "first",
      str_detect(
        phase_lower,
        "last quarter|last_quarter|last|third quarter|third_quarter"
      ) ~ "last",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(date)

intervals <- quarters %>%
  transmute(
    start = date,
    end = lead(date),
    phase_short,
    next_phase = lead(phase_short)
  ) %>%
  mutate(
    period = case_when(
      phase_short == "first" & next_phase == "last" ~ "full_moon_period",
      phase_short == "last" & next_phase == "first" ~ "new_moon_period",
      TRUE ~ NA_character_
    ),
    interval = end %--% start
  ) %>%
  filter(!is.na(period)) %>%
  select(start, end, period, interval)

fuzzy_left_join(
  data_eels,
  intervals,
  by = c("departure" = "interval"),
  match_fun = `%within%`
) %>%
  select(-c(start, end, interval)) -> data_eels_lunar


#plot circle diagram full_moon vs new_moon
p1 <- ggplot(data_eels_lunar, aes(x = label, fill = period)) +
  geom_bar(position = "dodge") +
  scale_fill_discrete(
    name = "Lunar Period",
    labels = c("Full Moon Period", "New Moon Period")
  ) +
  xlab("Behavioral State") +
  ylab("Number of Detections") +
  ggtitle("Detections during Lunar Periods") +
  theme_minimal()

#doesnt seem to have an effect
