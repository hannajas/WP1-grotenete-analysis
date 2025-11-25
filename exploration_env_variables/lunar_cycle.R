# Comparing the amount of detections in function of the lunar rhythm
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(wateRinfo)
library(tidyr)
library(tidyverse)
library(patchwork)
library(fuzzyjoin)
library(lubridate)
library(stats)

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


# -------------------------------------------------------------------
# 1. link lunar cycle data to detection data
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# 2. Detect all 4 quarter events (run EITHER 2. or 3.)
# -------------------------------------------------------------------
lunar_events <- lunar_cycle %>%
  mutate(phase_lower = tolower(moon_phase)) %>%
  mutate(
    event = case_when(
      str_detect(phase_lower, "full") ~ "full_moon",
      str_detect(phase_lower, "new") ~ "new_moon",
      str_detect(phase_lower, "first") ~ "first_quarter",
      str_detect(phase_lower, "last|third") ~ "last_quarter",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(event)) %>%
  arrange(date)

window_days <- 4

intervals <- lunar_events %>%
  mutate(
    start = date - days(window_days),
    end = date + days(window_days),
    interval = start %--% end
  ) %>%
  select(event, date, start, end, interval)

# -------------------------------------------------------------------
# 3.  Detect all 2 quarter events (run EITHER 2. or 3.) "near_full_moon and near_new_moon periods"
# -------------------------------------------------------------------
quarters <- lunar_cycle %>%
  mutate(phase_lower = tolower(moon_phase)) %>%
  filter(str_detect(
    phase_lower,
    "first quarter|first_quarter|first|last quarter|last_quarter|last|third quarter|third_quarter" #|full moon|full_moon|new moon|new_moon
  )) %>%
  mutate(
    phase_short = case_when(
      str_detect(phase_lower, "first quarter|first_quarter|first") ~ "first",
      str_detect(
        phase_lower,
        "last quarter|last_quarter|last|third quarter|third_quarter"
      ) ~ "last",
      # str_detect(
      #   phase_lower,
      #   "new moon|new_moon"
      # ) ~ "new",
      # str_detect(
      #   phase_lower,
      #   "full moon|full_moon"
      # ) ~ "full",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(date)

intervals <- quarters %>%
  transmute(
    start = date,
    end = dplyr::lead(date, n = 1),
    phase_short,
    next_phase = dplyr::lead(phase_short)
  ) %>%
  mutate(
    event = case_when(
      phase_short == "first" & next_phase == "last" ~ "full_moon_period",
      phase_short == "last" & next_phase == "first" ~ "new_moon_period",
      TRUE ~ NA_character_
    ),
    interval = end %--% start
  ) %>%
  filter(!is.na(event)) %>%
  select(start, end, event, interval)


# -------------------------------------------------------------------
# 4. link intervals to detection data
# -------------------------------------------------------------------
detection_type <- "arrival"
data_eels_lunar_raw <- fuzzy_left_join(
  data_eels,
  intervals,
  by = setNames("interval", detection_type),
  match_fun = `%within%`
) %>%
  select(-c(start, end, interval))

full_moons <- lunar_cycle %>%
  filter(!is.na(moon_phase) & str_detect(tolower(moon_phase), "full")) %>%
  pull(date) %>%
  unique() %>%
  sort()

# numeric vectors for fast indexing
fm_num <- as.numeric(full_moons)
dep_num <- as.numeric(data_eels_lunar_raw[[detection_type]])
idx <- findInterval(dep_num, fm_num) # 0 when departure is before first full moon
last_full <- ifelse(idx == 0, as.numeric(NA), fm_num[idx])
data_eels_lunar <- data_eels_lunar_raw %>%
  mutate(
    last_full_moon = as.POSIXct(
      last_full,
      origin = "1970-01-01",
      tz = attr(full_moons, "tz") %||% "UTC"
    ),
    days_since_last_full = round(as.numeric(difftime(
      data_eels_lunar_raw[[detection_type]],
      last_full_moon,
      units = "days"
    )))
    # days_since_last_full = interval(
    #   start = data_eels_lunar[[detection_type]],
    #   end = last_full_moon
    # )
  )


#plot circle diagram full_moon vs new_moon
p2 <- ggplot(data_eels_lunar, aes(x = days_since_last_full)) + #hier hoever van hoog en laag tij "x = days_since_last_full" OR x = hour(arrival
  geom_bar(aes(fill = event), color = "black") +
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = c(0, 5, 10, 15, 20, 25, 30),
    expand = c(0, 0)
  ) +
  # scale_fill_manual(
  #   name = "Lunar cycle",
  #   values = c("new_moon_period" = "#2E86C1", "full_moon_period" = "#F39C12"),
  #   labels = c(
  #     "new_moon_period" = "near new moon",
  #     "full_moon_period" = "near full moon"
  #   )
  # ) +
  style +
  labs(y = element_blank(), x = "days after full moon") +
  annotate(
    "text",
    x = 30, # place at "north" outer edge
    y = 28, # halfway up radial axis
    label = "# obs",
    angle = 90, # vertical orientation
    hjust = 0.4,
    vjust = 1.4,
    size = 11
  )
# facet_wrap(~zone)
print(p2)
ggsave("./figures/lunar/lunar_cycle_detections.png")

test <- data_eels_lunar %>%
  filter(
    event == "full_moon_period",
    days_since_last_full >= 10 &
      days_since_last_full <= 20
  )
# -------------------------------------------------------------------
# 5. Chi-squared test
# -------------------------------------------------------------------
Counts <- data_eels_lunar %>%
  group_by(event, zone) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = event, values_from = count, values_fill = 0)

proportions <- intervals %>%
  group_by(event) %>%
  summarise(
    total_duration = sum(as.numeric(int_length(interval)), na.rm = TRUE)
  ) %>%
  mutate(proportion = total_duration / sum(total_duration))

chi_test <- chisq.test(
  x = Counts[Counts$zone == 'tidal', 2:3],
  p = proportions$proportion
) #[Counts$zone == 'non-tidal',2:3]
chi_test$p.value
