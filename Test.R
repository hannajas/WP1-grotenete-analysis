library(tidyverse)
library(lubridate)
library(sf)
library(crawl)
library(ggspatial)
library(mapview)
library(prettymapr)
library(dbscan)
library(dplyr)
library(adehabitatLT)
library(factoextra)

data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)


########################################################################

a <- gdistance::shortestPath(transition, A, B, output = "SpatialLines")


# plot the speed of the fish for each segment
pdf("./figures/Downstream_segments_Tw_Q.pdf") # Create pdf
for (i in 2:length(seq_af) - 1) {
  print(paste("Processing segment:", seq_af[i], "to", seq_af[i + 1]))
  lag_station_name <- dplyr::lag(data_filter$station_name)
  print(paste("lag_station_name:", length(lag_station_name)))
  print(paste("station_name:", length(data_filter$station_name)))
  data_temp <- filter(
    data_filter,
    lag_station_name == seq_af[i] & station_name == seq_af[i + 1]
  )
  if (dim(data_temp)[1] == 0) {
    print(paste("No row found with this conditions, i = ", i))
  } else {
    print(paste("data_temp:", dim(data_temp)[1]))
    p1 <- ggplot(data_temp, aes(Tw, speed_m_s)) +
      geom_point(shape = 16, size = 5) +
      geom_smooth(method = lm, size = 2) +
      theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle = 90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25)
      ) +
      labs(x = "Temperature [°C]", y = "speed_m_s")

    p2 <- ggplot(data_temp, aes(Q, speed_m_s)) +
      geom_point(shape = 16, size = 5) +
      geom_smooth(method = lm, size = 2) +
      theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle = 90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25)
      ) +
      labs(x = "Discharge [m^3/s]", y = "speed_m_s")
    #print(p2 / p1 + plot_layout(heights = c(1,2)))
    print(p1)
  }
}
dev.off()

####################################################################################
# 2d to 1d
####################################################################################
#load shapefile

grotenete <- st_read("./data/geo_data/grotenete_zeeschelde.shp")
grotenete <- st_transform(grotenete, crs = 31370)
x <- 148246.11
y <- 198137.25
point <- st_sfc(st_point(c(x, y)), crs = st_crs(grotenete))
dist_to_river <- st_distance(point, grotenete, dist_fun =)
distance_from_source(
  x = x,
  y = y,
  river_shapefile = "./data/geo_data/grotenete_zeeschelde.shp",
  crs = 31370
)
#plot grotenete and point
plot(st_geometry(grotenete), col = 'lightblue')
plot(st_geometry(point), col = 'red', pch = 19, cex = 2, add = TRUE)



####################################################################################
# Circadian rhythm
####################################################################################
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
  ) %>%
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
      label == "resident/resting" &
        row_number() < first_migratory_idx ~
        "resident",
      label == "resident/resting" &
        row_number() > first_migratory_idx ~
        "resting",
      label == "migratory" ~ "migratory",
      is.infinite(first_migratory_idx) | is.na(first_migratory_idx) ~ "resident"
    )
  ) %>%
  ungroup() %>%
  filter(label == "migratory" | label == "resting")


circadian <- getSunlightTimes(
  as.Date(zes00a_1066_Q$Timestamp),
  lat = 51.216667,#same location as daylength
  lon = 4.6,
  keep = c("nightEnd", "sunrise", "sunset", "night")
)

# Define boundaries and phase names
phase_starts <- c("nightEnd", "sunrise", "sunset")
phase_ends   <- c("sunrise", "sunset", "night")
phase_names  <- c("dawn", "day", "dusk")

# For each day the duration of each phase
weights <- map2(phase_starts, phase_ends, ~ 
  as.duration(circadian[[.y]] - circadian[[.x]]) / ddays(1)
)
circadian[paste0("w_", phase_names)] <- weights
circadian$w_night <- 1 - rowSums(circadian[paste0("w_", phase_names)])
circadian$w_twilight <- circadian$w_dawn + circadian$w_dusk

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
periods <- c("dawn", "day", "dusk", "night")
events  <- c("arrival", "departure")

# Create all combinations of periods and events
grid <- expand.grid(period = periods, event = events, stringsAsFactors = FALSE)

# Iterate functionally
walk2(grid$period, grid$event, function(p, e) {
  col_name <- paste0(p, "_w_", substr(e, 1, 3))   # e.g. "dawn_w_arr"
  data_eels[[col_name]] <<- map_dbl(data_eels[[e]], function(x) {
    circadian[[paste0("w_", p)]][circadian$date == floor_date(x, "day")]
  })
})
#write.c