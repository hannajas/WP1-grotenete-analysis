# Handeling the bathymetric files obtained by Rachid Boulajhaf from DVW
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(marmap)
library(sf)
library(mapview)
#import .txt file in R with function:
file_path <- "./data/raw/cross_sections/240402_TAW_POS_multibeam_RUP.txt"
file_path_s <- "./data/raw/cross_sections/singlebeam_RUP.txt"
############################################################################################################
data <- read.table(file_path_s, col.names = c("x", "y", "depth"))
data_f <- data[data$x > 149310.72 & data$x < 149500.33, ]
#data <- read.table(file_path, col.names = c("x","y","depth"))
#data_f <- data[data$x > 149310.72 & data$x < 149500.33,]

#lambert_sf <- st_as_sf(data_f, coords = c("x", "y"), crs = 31370)
#latlon_sf <- st_transform(lambert_sf, crs = 4326)
#latlon_coords <- st_coordinates(latlon_sf)
#data_f[, c("x", "y")] <- latlon_coords
data_bat <- as.bathy(data_f)
#x_2 <- 51.085196
#y_2 <- 4.360276
#x_1 <- 51.083929
#y_1 <- 4.359135
x_2 <- 149420.1
x_1 <- 149298.54
y_2 <- 197294.99
y_1 <- 197084.59
test <- get.transect(data_bat, x_1, y_1, x_2, y_2, distance = TRUE)
ind <- c()
for (i in 1:length(test$lon)) {
  temp <- which.min((data_f$x - test$lon[i])^2 + (data_f$y - test$lat[i])^2)
  ind <- rbind(ind, temp)
}

data_cs <- data_f[unique(ind), ]
#de volledige rupel ingelezen met boot
# 149 324.55 m - 197 085.56 m
# 149 430.86 m - 197 271.06 m
line_vector <- c(x_2 - x_1, y_2 - y_1)
line_vector <- line_vector / sqrt(sum(line_vector^2)) # Normalize

# Project points onto the line
data_cs <- data_cs %>%
  mutate(
    projection = (x - x_1) * line_vector[1] + (y - y_1) * line_vector[2],
    x_proj = x_1 + projection * line_vector[1],
    y_proj = y_1 + projection * line_vector[2]
  )
ggplot(data_cs, aes(x = projection, y = depth)) +
  geom_line() +
  theme_minimal()

#Je kan enkel het profiel ONDER het water verkrijgen zo, als op moment van
# meting het peil laag was --> weinig info over snleheid bij hoge peilen....
ggplot(data_cs, aes(x = x, y = depth)) +
  geom_line() +
  theme_minimal()
#ggsave("./figures/cross_sections/multibeam_RUP.png")

##########################################################################################################
file_path_schelde_1 <- "C:/Code/WP1-grotenete-analysis/data/raw/cross_sections/240304_9626_SADL_MB_TAW_L72.txt" #schelde estuarium
file_path_schelde_2 <- "C:/Code/WP1-grotenete-analysis/data/raw/cross_sections/240318_9622_RMBU_MB_TAW_L72.txt" #schelde estuarium

schelde_MB <- read.table(file_path_schelde_2, col.names = c("x", "y", "depth"))
lambert_sf <- st_as_sf(schelde_MB, coords = c("x", "y"), crs = 31370)
latlon_sf <- st_transform(lambert_sf, crs = 4326)
latlon_coords <- st_coordinates(latlon_sf)
schelde_MB[, c("x", "y")] <- latlon_coords
data_bat <- as.bathy(latlon_sf)
x_1 <- 145683.037
y_1 <- 201463.070
x_2 <- 145964.518
y_2 <- 201417.037
test <- get.transect(data_bat, x_1, y_1, x_2, y_2, distance = TRUE)
ind <- c()
for (i in 1:length(test$lon)) {
  temp <- which.min((data_f$x - test$lon[i])^2 + (data_f$y - test$lat[i])^2)
  ind <- rbind(ind, temp)
}

data_cs <- data_f[unique(ind), ]
#de volledige rupel ingelezen met boot
# 149 324.55 m - 197 085.56 m
# 149 430.86 m - 197 271.06 m
line_vector <- c(x_2 - x_1, y_2 - y_1)
line_vector <- line_vector / sqrt(sum(line_vector^2)) # Normalize

# Project points onto the line
data_cs <- data_cs %>%
  mutate(
    projection = (x - x_1) * line_vector[1] + (y - y_1) * line_vector[2],
    x_proj = x_1 + projection * line_vector[1],
    y_proj = y_1 + projection * line_vector[2]
  )
ggplot(data_cs, aes(x = projection, y = depth)) +
  geom_line() +
  theme_minimal()

#Je kan enkel het profiel ONDER het water verkrijgen zo, als op moment van
# meting het peil laag was --> weinig info over snleheid bij hoge peilen....
ggplot(data_cs, aes(x = x, y = depth)) +
  geom_line() +
  theme_minimal()


########################################################################################################
#sf objects
schelde_MB <- st_as_sf(schelde_MB, coords = c("x", "y"), crs = 31370)
#mapView(schelde_MB)
plot(st_geometry(schelde_MB))

#create a line string for where you want your cross sextion
s1 <- rbind(c(145683.037, 201463.070), c(145964.518, 201417.037))
a_linestring <- st_linestring(s1)

cross_sec_id <- st_overlaps(schelde_MB, a_linestring)
cross_sec <- schelde_MB[cross_sec_id, ]
