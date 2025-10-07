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
# test R interpolation at for interpolated telemetry data
####################################################################################
data_inter_env <- read_csv(
  './data/interim/migration_inter.csv',
  show_col_types = FALSE
) %>%
  dplyr::select(-photoperiod)
var <- "R"

temp <- align_resolutions_function(
  #R --> all data on 15 min
  var,
  as.difftime(15, units = "mins"), #as.period(5, "mins"),
  metadata,
  upsample_method = "ffill",
  data_inter_env
)
p <- 1
# int_test <-
#   inverse_distance(
#     data_inter_env,
#     temp,
#     metadata,
#     p,
#     var
#   )
env_data <- temp
metadata_filter <- filter(metadata, metadata$type == var)


n <- dim(metadata_filter)[1]
V <- as.matrix(env_data)
nan_V <- which(is.nan(V))
telemetry_data <- data_inter_env
W <- lapply(1:n, function(i) {
  distance <- distm(
    telemetry_data[c("inter_longitude", "inter_latitude")],
    metadata_filter[c("station_longitude", "station_latitude")][i, ],
    fun = distHaversine
  )
  ifelse((distance == 0), NaN, abs(1 / (distance)^p))
})

distance <- distm(
  telemetry_data[c("inter_longitude", "inter_latitude")],
  metadata_filter[c("station_longitude", "station_latitude")][1, ],
  fun = distHaversine
)
View(distance)
