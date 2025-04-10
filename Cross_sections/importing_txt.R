library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(marmap)
library(sp)
library(sf)
#import .txt file in R with function:
file_path <- "./data/raw/cross_sections/240402_TAW_POS_multibeam_RUP.txt"
data <- read.table(file_path, col.names = c("x","y","depth"))
data_f <- data[data$x > 149310.72 & data$x < 149500.33,]
#lambert_sf <- st_as_sf(data_f, coords = c("x", "y"), crs = 31370)
#latlon_sf <- st_transform(lambert_sf, crs = 4326)
#latlon_coords <- st_coordinates(latlon_sf)
#data_f[, c("x", "y")] <- latlon_coords
data_bat <- as.bathy(data_f)
#x_2 <- 51.085196
#y_2 <- 4.360276
#x_1 <- 51.083929
#y_1 <- 4.359135
x_2 <- 149430.86
x_1 <- 149324.55
y_2 <- 197271.06
y_1 <- 197085.56
test <- get.transect(data_bat,x_1,y_1,x_2,y_2, distance = TRUE)
ind <- c()
for (i in 1:length(test$lon)){
  temp <- which.min((data_f$x - test$lon[i])^2 + (data_f$y-test$lat[i])^2)
  ind <- rbind(ind, temp)
}

data_cs <- data_f[unique(ind),]
#de volledige rupel ingelezen met boot
# 149 324.55 m - 197 085.56 m
# 149 430.86 m - 197 271.06 m
line_vector <- c(x_2 - x_1, y_2 - y_1)
line_vector <- line_vector / sqrt(sum(line_vector^2))  # Normalize

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