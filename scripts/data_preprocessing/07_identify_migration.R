#' Identify migration behavior
#' 
#' Damiano Oldoni (damiano.oldoni@inbo.be)
#' Pieterjan Verhelst (pieterjan.verhelst@inbo.be)
#' 
#' R script for identifying eel migration behavior based on discussion in
#' https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/17
#' 

library(readr)      # to read csv files
library(dplyr)      # to do data wrangling
library(tidyr)      # to organize tabular data
library(tidylog)    # to get useful messages about dplyr and tidyr operations
library(purrr)      # to do functional programming
library(ggplot2)    # to create plots
library(plotly)     # to make ggplot plots interactive

source("src/identify_migration_functions.R")

# read input data
eel_df <- read_csv("./data/interim/speed.csv")
eel_df <- eel_df %>%
  rename(row_id = "...1")


# define thresholds
dist_for_speed <- 4000 # threshold in meter
migration_speed_threshold <- 0.01 # speed threshold in m/s
stationary_range <- 1005 #threshold in meter defining what we consider as "stationary"

# Apply get_migrations to each eel
eel_df <- eel_df %>%
  group_by(tag_serial_number) %>%
  nest() %>%
  mutate(migration_infos = map(data, function(x) {
    get_migrations(x, 
                   dist_threshold = dist_for_speed,
                   speed_threshold = migration_speed_threshold,
                   smooth_threshold = stationary_range)
  }), .keep = "none") %>%
  unnest(migration_infos)

#View(eel_df)


# select one eel
tag_serial_number_example <- "1171746"
eel_example <- eel_df %>%
  filter(tag_serial_number == tag_serial_number_example)
# plot
plot_example <- ggplot(eel_example, aes(x = arrival, y = distance_to_source_m)) +
  geom_point(aes(color = migration), size = 3) +     # color according to 'downstream_migration' or 'has_migration_started'
  geom_line() + 
  scale_y_reverse() +
  ggplot2::labs(title = sprintf("%s, speed: %s, min dist for speed calc: %s", 
                                tag_serial_number_example,
                                migration_speed_threshold,
                                dist_for_speed)
  )

plot_example

# interactive plot version
ggplotly(plot_example)

# write.csv
write.csv(eel_df, "./data/interim/migration.csv")


