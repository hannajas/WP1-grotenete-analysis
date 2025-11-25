library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)
library(geosphere)

# load data
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE) %>%
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
ggplot(aes(x = cluster, y = speed_m_s,color= factor(cluster))) +
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
# Split data by tag_serial_number
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
  if (length(cond) != nrow(df)) cond <- rep(cond, length.out = nrow(df))
  idx <- which(cond)
  if (length(idx) == 0) return(df[0, , drop = FALSE])
  keep <- unique(c(idx - 1, idx))
  keep <- keep[keep >= 1 & keep <= nrow(df)]
  df[keep, , drop = FALSE]
}

# Apply to each eel
average_speeds <- lapply(data_list, function(x) {
  list(
    total = compute_avg_speed(x),
    resident = compute_avg_speed(select_rows_with_neighbors(x, x$label == "resident")),
    migration = compute_avg_speed(select_rows_with_neighbors(x, x$label == "migration")),
    non_tidal_m = compute_avg_speed(select_rows_with_neighbors(x, x$zone == "non-tidal" & x$label == "migration")),
    tidal_m = compute_avg_speed(select_rows_with_neighbors(x, x$zone %in% c("tidal") & x$label == "migration")),
    transition_m = compute_avg_speed(select_rows_with_neighbors(x, x$zone %in% c("transition") & x$label == "migration"))
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
    cols = c(resident,migration,non_tidal_m, transition_m, tidal_m),
    names_to = "zone_type",
    values_to = "average_speed"
  )


#########################################################################################################
#visualize
data %>%
group_by(tag_serial_number, cluster, zone) %>%
  summarise(speed_m_s = mean(speed_m_s, na.rm = TRUE)) %>%
  ungroup()
  ggplot(aes(y = speed_m_s)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.color = "red") +
  #geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Average swimming speeds by zone type",
    x = "Zone type",
    y = "Average speed (m/s)"
  ) +
  theme_minimal(base_size = 14)

ggplot(average_speeds_long, aes(x = zone_type, y = average_speed)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.color = "red") +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Average swimming speeds by zone type",
    x = "Zone type",
    y = "Average speed (m/s)"
  ) +
  theme_minimal(base_size = 14)

summary_speed <- summary(average_speeds_df)
#get standard deviation of average speeds
sd_speed <- sapply(average_speeds_df[, -1], sd, na.rm = TRUE)
print(sd_speed)

average_speed_df <- average_speeds_df %>%
  #rename(tag_serial_number = .id) %>%
  mutate(tag_serial_number = as.numeric(tag_serial_number))
eeldata <- left_join(metadata_eel, average_speed_df, by = "tag_serial_number")

eeldata %>%
  ggplot(aes(x = length1, y = tidal)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method = "lm")


# other visualizations
# density plot of the average speed
data %>%
  group_by(tag_serial_number) %>%
  summarise(mean_speed = mean(migration_speed, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_speed)) +
  geom_density() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# link weight and length
ggplot(eeldata) +
  geom_point(aes(x = length1, y = weight)) +
  geom_smooth(method = "lm")


#plot density plot of migration_speed
data %>%
  ggplot(aes(
    x = speed_m_s,
    color = downstream_migration,
    fill = downstream_migration
  )) +
  geom_density(alpha = 0.7)


# plot speed distribution
# plot the speed distribution
g <- ggplot()
g <- g +
  theme(
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
g <- g +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
g <- g +
  geom_density(
    aes(x = speed_m_s),
    data = speed,
    fill = "#69b3a2",
    color = "#e9ecef",
    alpha = 0.8
  )
g <- g + scale_x_log10(guide = "axis_logticks")
g <- g + geom_vline(xintercept = 0.01, linetype = "dotted", linewidth = 1)
print(g)
# ggsave('./figures/speed_m_s_distribution.png')
