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
# test new inverse distance interpolation
####################################################################################
data_type <- "Q"
telemetry_data <- data
temp <- concat_all_env_vars(data, "Q", metadata)
env_data <- temp
metadata_filter <- filter(metadata, metadata$type == data_type)
p <- 2
look_up_table <- lookup


n <- dim(metadata_filter)[1]
V <- as.matrix(env_data)
nan_V <- which(is.nan(V))

# find two closest recievers (upstream and downstream)

W <- lapply(seq_along(telemetry_data$interpolation_location), function(j) {
  diffs <- metadata_filter$distance_to_source - telemetry_data$interpolation_location[j]

  # upstream (closest negative diff) and downstream (closest positive diff)
  upstream_idx <- if (any(diffs < 0)) which.max(diffs[diffs < 0]) else NA
  downstream_idx <- if (any(diffs > 0)) which.min(diffs[diffs > 0]) else NA

  selected_idx <- na.omit(c(
    if (!is.na(upstream_idx)) which(diffs == max(diffs[diffs < 0]))[1],
    if (!is.na(downstream_idx)) which(diffs == min(diffs[diffs > 0]))[1], #of NA for upstream_idx or downstream_idx is NA --> choose closest environmental station
    if (is.na(upstream_idx) | is.na(downstream_idx)) which.min(abs(diffs)) #if no upstream or downstream station, choose closest station
  ))

  w <- rep(0, length(diffs)) # start with zeros
  if (length(selected_idx) == 2) {
    w[selected_idx] <- 1 / abs(diffs[selected_idx])^p
  } else if (length(selected_idx) == 1) {
    w[selected_idx] <- 1
  }
  return(w)
})

W <- matrix(unlist(W), ncol = n, byrow = TRUE)

  # dealing with the nan values in temperature values
  W[nan_V] <- 0


ind_row_gn <- which(telemetry_data$river_segment == "gn")
ind_col_gn <- which(metadata_filter$segment != "gn")
ind_row_rup <- which(telemetry_data$river_segment == "rup")
ind_col_rup <- which(metadata_filter$segment != "rup")
ind_row_zes <- which(
  telemetry_data$river_segment == "zes_up" |
    telemetry_data$river_segment == "zes_down"
)
ind_col_zes <- which(metadata_filter$segment != "zes")
W[ind_row_gn, ind_col_gn] <- 0
W[ind_row_rup, ind_col_rup] <- 0
W[ind_row_zes, ind_col_zes] <- 0