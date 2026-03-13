library(tidyverse)
library(dplyr)
library(sp)
library(sf)
library(adehabitatLT)
library(factoextra)
library(usethis)

##############################################################################################################
# Load data
##############################################################################################################
data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)


##############################################################################################################
# kmeans with raw data - each eel separately
##############################################################################################################
data$cluster <- NA
# data <- data %>%
#   filter(migration == TRUE)
mydfnew.split.eel <- split(data, data$tag_serial_number)

#no visualisation
for (i in 1:length(mydfnew.split.eel)) {
  mydfnew.temp <- mydfnew.split.eel[[i]] %>%
    filter(!is.na(speed_m_s)) #%>%
  # filter(migration == TRUE)
  if (nrow(mydfnew.temp) < 2) {
    next
  }
  result <- kmeans(log(mydfnew.temp$speed_m_s), centers = 2, nstart = 10)

  # Reassign cluster labels so that cluster 1 always has the lower center
  centers <- result$centers
  cluster_map <- if (centers[1] < centers[2]) c(1, 2) else c(2, 1)
  reassigned_clusters <- cluster_map[result$cluster]

  mydfnew.temp$cluster <- as.factor(reassigned_clusters)

  # Ensure cluster column aligns with the original data
  mydfnew.split.eel[[i]]$cluster <- NA
  # mydfnew.split.eel[[i]]$cluster[
  #   !is.na(mydfnew.split.eel[[i]]$speed_m_s)
  # ] <- mydfnew.temp$cluster
  mydfnew.split.eel[[i]]$cluster[
    !is.na(mydfnew.split.eel[[i]]$speed_m_s) #&
    #mydfnew.split.eel[[i]]$migration == TRUE
  ] <- mydfnew.temp$cluster
  mydfnew.split.eel[[i]]$cluster[
    lag(mydfnew.split.eel[[i]]$migration) == FALSE
  ] <- 1
  if ((mydfnew.split.eel[[i]]$migration[1]) == TRUE) {
    mydfnew.split.eel[[i]]$cluster[1] <- 2
  }
  onset_id <- which(mydfnew.split.eel[[i]]$migration == TRUE)[1] + 1
  while (
    !is.na(onset_id) &&
      onset_id > 2 &&
      mydfnew.split.eel[[i]]$cluster[onset_id] ==
        mydfnew.split.eel[[i]]$cluster[onset_id - 1]
  ) {
    onset_id <- onset_id + 1
  }
  if (onset_id <= 2 || is.na(onset_id)) {
    next
  }
  mydfnew.split.eel[[i]]$cluster[onset_id] <- 2
}
data_cluster <- bind_rows(mydfnew.split.eel)
#save as csv
write_csv(data_cluster, "./data/interim/migration_env_filter.csv")





pdf("./figures/Clustering/kmeans_log_sep_eels_test.pdf")
mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs

for (eel in names(mydfnew.split.eel)) {
  mydfnew <- mydfnew.split.eel[[eel]]
  # only if more than 1 row
  # if (!("O" %in% names(mydfnew)) || all(is.na(mydfnew$Q))) {
  #   next
  # }
  plot <- plot_one_traj(
    mydfnew,
    "none"
  )
  print(plot)
}
dev.off()







mydfnew.split.eel <- split(data_cluster, data$tag_serial_number)
pdf("./figures/Clustering/kmeans_log_sep_eels_test.pdf")
for (i in 1:length(mydfnew.split.eel)) {
  mydfnew.temp <- mydfnew.split.eel[[i]]
  mydfnew.temp$cluster <- as.factor(mydfnew.temp$cluster)
  g <- ggplot(mydfnew.temp)
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
    geom_line(
      aes(arrival, -1 * distance_to_source_m),
      colour = "black",
      linewidth = 1
    )
  g <- g +
    geom_point(
      aes(arrival, -1 * distance_to_source_m, colour = cluster),
      shape = 16,
      size = 5
    )
  g <- g + scale_color_manual(values = c("red", "green", "grey"))
  #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
  g <- g +
    theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20))
  #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
  g <- g + labs(title = mydfnew.temp$tag_serial_number[1]) #, subtitle = mydfnew.temp$catch_year)
  g <- g + ylab("Distance (m)")
  g <- g + xlab("Date")
  # g <- g + scale_x_date(date_labels = "%b-%d-%Y")
  g <- g + scale_x_datetime(date_breaks = "1 week")
  g <- g +
    geom_hline(
      yintercept = -1 * mydfnew.temp$distance_to_source_m,
      colour = "gray",
      linewidth = 0.5,
      linetype = "dashed"
    )
  g <- g +
    annotate(
      "text",
      x = mydfnew.temp$arrival[1],
      y = -1 * mydfnew.temp$distance_to_source_m,
      label = mydfnew.temp$station_name,
      hjust = 0,
      colour = "red",
      size = 3
    )
  g <- g + theme(legend.position = "bottom")
  print(g)
}
dev.off()