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


######################################
data_inter_env$station_name == metadata_filter$receiver[1]

find_receiver <- data_inter_env$station_name == metadata_filter$receiver[1]
test <- replace(find_receiver, is.na(find_receiver), FALSE)

test <- replace(
  telemetry_data$temp,
  telemetry_data$station_name == metadata_filter$receiver[i],
  unlist(env_data[
    telemetry_data$station_name == metadata_filter$receiver[i],
    i
  ])
)


#######################################################################################
#interpolation
#######################################################################################
m <- 8
inter.temp <- data_inter.eel[[m]]
data.temp <- data.eel.filtered[[m]]

original_times <- data.temp$rounded_date
new_times <- inter.temp$date

#is_original <- floor_date(new_times, unit = resolution_s) %in% floor_date(original_times, unit = resolution_s) # check if the date in inter.temp is in the original data
is_original <- new_times %in% original_times

row_ids <- which(data_inter$tag_serial_number == names(data_inter.eel)[m]) #select the row id's in data_inter of id k

data_inter$is_original[row_ids] <- as.integer(is_original)



View(data_inter[data_inter$tag_serial_number == names(data_inter.eel)[7], ])


data_test <- left_join(
  data_inter[data_inter$tag_serial_number == names(data_inter.eel)[m], c("x", "y", "date", "dist", "dt", "tag_serial_number")],
  data[data$tag_serial_number == names(data_inter.eel)[m], setdiff(names(data), c("middledate", "x", "y"))],
  by = c("tag_serial_number" = "tag_serial_number", "date" = "rounded_date")
)