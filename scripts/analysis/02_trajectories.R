# Calculation durations and speeds of the eel trajectories
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(tidyquant)
library(patchwork)
library(geosphere)

# load data
data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  group_by(tag_serial_number) %>%
  mutate(
    label = ifelse(
      cluster == 1,
      "resident/resting",
      ifelse(cluster == 2, "migratory", NA)
    )
  ) %>%
  mutate(
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE),
    year = year(arrival[1]),
    month = month(arrival[1]),
    month_onset = month(arrival[first_migratory_idx[1]]),
  ) %>%
  mutate(
    label = case_when(
      label == "resident/resting" &
        row_number() < first_migratory_idx ~
        "resident",
      label == "resident/resting" &
        row_number() > first_migratory_idx ~
        "migration",
      label == "migratory" ~ "migration",
      is.infinite(first_migratory_idx) | is.na(first_migratory_idx) ~ "resident"
    )
  ) %>%
  mutate(
    swimdistance_m = if_else(is.na(swimdistance_m), 0, swimdistance_m),
    cumsum_swim_dist = cumsum(swimdistance_m)
  ) %>%
  ungroup()
metadata_eel <- read_csv('./data/raw/eel_meta_data.csv', show_col_types = FALSE)
metadata <- read_csv('./data/interim/Metadata.csv', show_col_types = FALSE)
# data <- filter(
#   data,
#   (!startsWith(data$station_name, "ws-") & data$downstream == TRUE)
# )
data %>%
  ggplot(aes(x = cluster, y = speed_m_s, color = factor(cluster))) +
  geom_boxplot()

#boxplot of speed for each SEGEMENT (seg_id)
# data %>%
#   ggplot(aes(
#     x = reorder(seg_id, distance_to_source_m, FUN = mean, na.rm = TRUE),
#     y = migration_speed
#   )) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########################################################################################################
# speed tidal, non-tidal and whole trip
data_list <- split(data, f = data$tag_serial_number)

# Function to compute average speed for a subset of data
compute_avg_speed <- function(df) {
  # df <- df %>%
  #   filter(downstream_migration == TRUE)
  if (nrow(df) < 1) {
    return(NA)
  }
  if (nrow(df) == 1) {
    return(df$speed_m_s)
  }
  swim_distance <- tail(df$cumsum_swim_dist, n = 1) - df$cumsum_swim_dist[1]
  swim_time <- as.numeric(difftime(
    tail(df$departure, n = 1),
    df$departure[1],
    units = "secs"
  ))
  avg_speed <- swim_distance / swim_time
  return(avg_speed)
}

select_rows_with_neighbors <- function(df, cond) {
  if (length(cond) != nrow(df)) {
    cond <- rep(cond, length.out = nrow(df))
  }
  idx <- which(cond)
  if (length(idx) == 0) {
    return(df[0, , drop = FALSE])
  }
  keep <- unique(c(idx - 1, idx))
  keep <- keep[keep >= 1 & keep <= nrow(df)]
  df[keep, , drop = FALSE]
}

# Apply to each eel
average_speeds <- lapply(data_list, function(x) {
  list(
    total = compute_avg_speed(x),
    resident = compute_avg_speed(select_rows_with_neighbors(
      x,
      x$label == "resident"
    )),
    migration = compute_avg_speed(select_rows_with_neighbors(
      x,
      x$label == "migration"
    )),
    non_tidal_m = compute_avg_speed(select_rows_with_neighbors(
      x,
      x$zone == "non-tidal" & x$label == "migration"
    )),
    tidal_m = compute_avg_speed(select_rows_with_neighbors(
      x,
      x$zone %in% c("tidal") & x$label == "migration"
    )),
    transition_m = compute_avg_speed(select_rows_with_neighbors(
      x,
      x$zone %in% c("transition") & x$label == "migration"
    ))
  )
})

# Convert to a data frame for easier viewing
average_speeds_df <- do.call(
  rbind,
  lapply(names(average_speeds), function(name) {
    cbind(tag_serial_number = name, as.data.frame(average_speeds[[name]]))
  })
)

average_speeds_long <- average_speeds_df %>%
  pivot_longer(
    cols = c(resident, migration, non_tidal_m, transition_m, tidal_m),
    names_to = "zone_type",
    values_to = "average_speed"
  )

anova_model <- aov(
  average_speed ~ zone_type,
  data = filter(average_speeds_long, zone_type %in% c("non_tidal_m", "tidal_m"))
)
summary(anova_model)

summary_speed <- summary(average_speeds_df)
#get standard deviation of average speeds
sd_speed <- sapply(average_speeds_df[, -1], sd, na.rm = TRUE)
print(sd_speed)

average_speed_df <- average_speeds_df %>%
  #rename(tag_serial_number = .id) %>%
  mutate(tag_serial_number = as.numeric(tag_serial_number))
eeldata <- left_join(metadata_eel, average_speed_df, by = "tag_serial_number")


#########################################################################################################
# speed tidal, non-tidal and whole trip
mig_durations <- data %>%
  group_by(tag_serial_number) %>%
  summarise(
    migration_duration = as.numeric(difftime(
      max(departure[label == "migration"], na.rm = TRUE),
      if (any(label == "resident", na.rm = TRUE)) {
        max(departure[label == "resident"], na.rm = TRUE)
      } else {
        min(arrival, na.rm = TRUE)
      },
      units = "days"
    ))
  ) %>%
  mutate(
    migration_duration = ifelse(
      is.infinite(migration_duration),
      NA,
      migration_duration
    )
  )
mean(mig_durations$migration_duration, na.rm = TRUE)
sd(mig_durations$migration_duration, na.rm = TRUE)

tot_durations <- data %>%
  group_by(tag_serial_number) %>%
  summarise(
    total_duration = as.numeric(difftime(
      max(departure, na.rm = TRUE),
      min(arrival, na.rm = TRUE),
      units = "days"
    ))
  ) %>%
  mutate(
    total_duration = ifelse(is.infinite(total_duration), NA, total_duration)
  )
mean(tot_durations$total_duration, na.rm = TRUE)
sd(tot_durations$total_duration, na.rm = TRUE)
