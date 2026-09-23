# Importing environmental data by making use of the wateRinfo package
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library("sf")
library("raster")
library("mapview")
library("leaflet")
library(terra)

source("./src/coordinate_to_river1D_function.R")
source("./src/distance_from_source_to_coordinate_function.R")


#########################################################################################################################
#get distance from source
#########################################################################################################################
# source = most upstream release location of the river segment

#load csv
lookup_gis <- read_csv(
  './data/geo_data/grotenete_zeeschelde_lookup_Lambert_gis.csv', #QGIS: line vector --> points (point along geometry), extract coordinates of these points (add geometry attributes)
  show_col_types = FALSE
)

#Distance to
dist_splits_1 <- lookup_gis %>%
  filter(NAAM == "Grote Nete") %>%
  dplyr::select(distance) %>%
  max() #grote nete --> rupel

dist_af_splits <- 60363 #zeescheldt afwaartst, zeescheldt opwaarts

#rename each river segment gn, rup, zes_up, zes_down
lookup_gis <- lookup_gis %>%
  mutate(
    NAAM = case_when(
      NAAM == "Zeeschelde" & distance > dist_af_splits ~ "zes_down",
      NAAM == "Zeeschelde" & distance <= dist_af_splits ~ "zes_up",
      NAAM == "Rupel" ~ "rup",
      NAAM == "Grote Nete" ~ "gn",
      TRUE ~ NAAM
    ),
    distance = case_when(
      NAAM == "rup" ~ distance + dist_splits_1 + 1,
      TRUE ~ distance
    )
  )

#afstand tot splitsing
dist_splits_2 <- lookup_gis %>%
  filter(NAAM == "rup") %>%
  dplyr::select(distance) %>%
  max() #Rupel --> Zeescheldt


zee_op <- lookup_gis %>%
  filter(NAAM == "zes_down") %>%
  mutate(distance_to_source = distance + dist_splits_2 - dist_af_splits)

zee_af <- lookup_gis %>%
  filter(NAAM == "zes_up") %>%
  mutate(distance_to_source = rev(distance) + dist_splits_2 + 1)

rest <- lookup_gis %>%
  filter(NAAM != "zes_up" & NAAM != "zes_down") %>%
  mutate(distance_to_source = distance)

look_up_corr <- rbind(zee_op, zee_af, rest)

##############################################################################################
# Calculate the distance to source for each receiver
#load deployments
deployments <- read_csv(
  './data/external/receivernetwork_2019_Grotenete.csv',
  show_col_types = FALSE
) %>% #filter missing values out of coordinates
  filter(!is.na(latitude) & !is.na(longitude))
#to sf
deployments_sf <- st_as_sf(
  deployments,
  coords = c("longitude", "latitude"),
  crs = 4326
) %>%
  st_transform(crs = 31370)
#look_up_corr to sf
look_up_corr_sf <- st_as_sf(
  look_up_corr,
  coords = c("xcoord", "ycoord"),
  crs = 31370
)

#find for each deployment the closest lookup point
dist_matrix <- st_distance(deployments_sf, look_up_corr_sf)
min_indices <- apply(dist_matrix, 1, which.min)
#add distance to source to deployments
deployments$distance_to_source_m <- look_up_corr$distance_to_source[min_indices]
deployments$distance <- look_up_corr$distance[min_indices]
#write deployments
write_csv(deployments, './data/geo_data/deployments_distance_to_source.csv')

#save as csv
write_csv(
  look_up_corr,
  "./data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv"
)
