library("remotes")
library("actel")
library(tidyverse)
library(RSP)
library(dplyr)
library(sf)
library(mapview)

#######################################################################################################################################################
#Detections (LongLat)
detections <- read_csv(
  './data/raw/raw_detection_data.csv',
  show_col_types = FALSE
) %>%
  #keep only the receivers in Grote Nete network (exclusive estuarium ws-xx)
  filter(acoustic_project_code != "bpns") %>%
  filter(acoustic_project_code != "CONNECT-MED") %>%
  filter(acoustic_project_code != "Orstedcod") %>%
  filter(acoustic_project_code != "SPAWNSEIS") %>%
  filter(acoustic_project_code != "cpodnetwork") %>%
  filter(acoustic_project_code != "OP-Test") %>%
  filter(acoustic_project_code != "MOBEIA") %>%
  filter(!startsWith(station_name, "ws"))
detections$Receiver <- unlist(map(strsplit(detections$receiver_id, "-"), 2)) #receiver_id from bv. VR2W-113528 --> 113528

stations_rel <- c(
  stations,
  "rel_grotenete1",
  "rel_grotenete2",
  "rel_grotenete3"
)

#get receiver ID's in same order as stations
#make sure there is a one-to-one link between stations and receivers
stations <- unique(detections$station_name)
receiver_id <- c()
for (i in 1:length(stations)) {
  filter_det <- detections[detections$station_name == stations[i], ]
  link <- unique(filter_det$Receiver)
  if (length(link) > 1) {
    #s-6 --> 122354 (deze wordt verwijderd), 115441 en s-9 --> 113528 (deze wordt verwijderd), 122319
    print(paste("stations:", stations[i], "receivers:", link))
    receiver_id <- c(receiver_id, link[1]) #else
    detections$Receiver[detections$station_name == stations[i]] <- link[1]
  } else {
    receiver_id <- c(receiver_id, link)
  }
}

#preparing detection data for input actel data
movements <- detections %>%
  rename(Timestamp = date_time)
movements$CodeSpace <- extractCodeSpaces(movements$acoustic_tag_id)
movements$Signal <- extractSignals(movements$acoustic_tag_id)
write_csv(movements, './data/actel/detections/detections.csv')


###########################################################################
# Spatial (LongLat + Lambert)
deployments_raw <- read_csv(
  './data/geo_data/deployments_projected.csv',
  show_col_types = FALSE
) %>%
  #filter op zelfde stationnames als in detections.csv
  filter(station_name %in% stations) %>%
  rename(
    Station.name = station_name,
    Latitude = deploy_latitude,
    Longitude = deploy_longitude
  ) %>%
  select(-c("...1"))
deployments_raw$Type <- "Hydrophone"
#no range column because the default of 500m is used here

release <- data.frame(
  animal_project_code = c("2019_Grotenete", "2019_Grotenete", "2019_Grotenete"),
  Station.name = c("rel_grotenete1", "rel_grotenete2", "rel_grotenete3"),
  X = NA,
  Y = NA,
  Longitude = c(5.003906, 4.997392, 4.996931),
  Latitude = c(51.144779, 51.139997, 51.138881),
  Type = c("Release", "Release", "Release")
)

cord.dec <- st_as_sf(
  release[, c("Longitude", "Latitude")],
  crs = 4326,
  coords = c("Longitude", "Latitude")
)
lambert <- st_transform(cord.dec, crs = 31370)
release[, c("X", "Y")] <- st_coordinates(lambert)

spatial <- rbind(deployments_raw, release)

#spatial <- left_join(distance_matrix,spatial, by=c("...1"="Station.name")) %>%
#    select(-all_of(c(stations,"rel_grotenete1","rel_grotenete2","rel_grotenete3"))) %>%
#    rename(Station.name = ...1)
spatial$Array <- "A1"
spatial$Section <- "River"
#handmatige aanpassingen niet meer nodig door projectie van receivers op tallijn
#spatial[spatial$Station.name =="bn-2","Latitude"] <- 51.117187
#spatial[spatial$Station.name =="gn-14","Longitude"] <- 4.996392
#spatial[spatial$Station.name =="gn-10","Latitude"] <- 51.091483
#spatial[spatial$Station.name =="gn-10","Longitude"] <- 4.945819

#write_csv(spatial, './data/actel/spatial.csv')
write_csv(spatial, './spatial.csv')


###########################################################################
# distance matix
#load distance matrix
distance_matrix <- read_csv(
  './data/raw/distancematrix_2019_grotenete.csv',
  show_col_types = FALSE
)
#rename the distance matrix
spatial_standard_name <- loadSpatial('./data/actel/spatial.csv')
distance_matrix <- distance_matrix %>%
  filter(...1 %in% stations_rel)
distance_matrix <- distance_matrix %>%
  select(all_of(c("...1", distance_matrix$...1)))
distance_matrix$...1 <- spatial_standard_name$Standard.name[match(
  colnames(distance_matrix)[-1],
  spatial_standard_name$Station.name
)]
#rename the columns of the distance matrix as spatial_standard_name$Standard.name:
colnames(distance_matrix) <- c(
  "...1",
  spatial_standard_name$Standard.name[match(
    colnames(distance_matrix)[-1],
    spatial_standard_name$Station.name
  )]
)
#nu geeft ...1 de kolomnaam NA
write_csv(distance_matrix, './data/actel/distances.csv')


##########################################################################
#biometry - data on eels and their tags (release and capture LatLong)
eel_meta_data <- read_csv(
  './data/raw/eel_meta_data.csv',
  show_col_types = FALSE
)
biometry <- eel_meta_data %>%
  rename(
    Signal = acoustic_tag_id,
    Release.date = release_date_time,
    Release.site = release_location,
    Serial.nr = tag_serial_number
  ) %>%
  mutate(
    Release.date = as.POSIXct(
      dmy_hm(Release.date),
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    ),
    Signal = extractSignals(eel_meta_data$acoustic_tag_id),
    Code.space = extractCodeSpaces(eel_meta_data$acoustic_tag_id)
  )
biometry$Code.space[grepl(",", biometry$Code.space)] <- unlist(map(
  strsplit(unique(biometry$Code.space), ","),
  2
))
biometry$Release.site <- gsub(
  'Downstream confluent Grote Nete and Molse Nete',
  'rel_grotenete1',
  gsub(
    'Grote Nete 200 m upstream deployment gn-14',
    'rel_grotenete2',
    gsub('Wildersedijk Grote Nete', 'rel_grotenete3', biometry$Release.site)
  )
)
write_csv(biometry, './data/actel/biometrics.csv')


#########################################################################
# deployments (no geo reference)
deployments <- data.frame(Station.name = stations)
deployments$Receiver <- receiver_id
deployments$Start <- min(biometry$Release.date)
deployments$Stop <- max(biometry$Release.date) + days(85)
write_csv(deployments, './data/actel/deployments.csv')


test <- read_csv('./data/actel/detections.csv')
#create an actel object
x <- preload(
  spatial = './data/actel/spatial.csv',
  detections = './data/actel/detections/detections.csv', #heeft problemen met detections.csv --> zogezegd verschillende kolomen die er niet zijn: Timestamp, Receiver, CodeSpace, Signal
  biometrics = './data/actel/biometrics.csv',
  deployments = './data/actel/deployments.csv',
  distances = './data/actel/distances.csv',
  tz = "UTC"
)

#aangezien preload niet lukt dan toch maar csv's in deze folder
explore_out <- explore(
  tz = "UTC",
  GUI = "never",
  max.interval = 60000,
  speed.method = "last to last"
) #save.tables.locally = TRUE
# max time mag heel hoog --> we willen niet dat hij een nieuwe track begint

###########################################################################
# RSP
#aim of this RSP package is to create BBMMs as input for further statististical analysis
# (The coordinates of your receivers and release sites in the same coordinate system as the shapefile.)

#raster
#base.raster_lamb <- shapeToRaster("./data/raw/shape/Grote_nete_water.shp", size =5, coord.x = "X", coord.y="Y", type = "water")
base.raster_longlat <- terra::rast(
  "C:/Users/hjaspaer/OneDrive - UGent/Documents/Werkpakkket I/GIS/raster_water_extended_longlat.tif"
)

#export a raster to .tif with terra
#terra::writeRaster(base.raster, "./raster_5m.tif", overwrite=TRUE)
#TO MANY RECEIVERS ON THE LAND!Warning: Stations starting with 'ws' AND 'gn-11', 'gn-10', 'bn-2', 'bn-Walem', 'ak-41', 's-4a','s-8','s-9','s-6' are not placed in water!

#DUS ws detecties uit detecties knippen
#shapefile uitgebreidt zodat s-6, s-8,ak-41,gn-11','gn-10', 'bn-2', 'bn-Walem' in detectie gebied liggen
#behalve 'bn-2' --> ligt te ver buiten shape (op het land?) --> van coordinaten verplaatst

raster::plot(base.raster_longlat, col = "blue") #niets miss met .tif file R plot het meteen naar hij verlaagt de resolutie
#resolutie figuur verhogen miss in raster package proberen --> DPI verhoger
#hv plot plotly in python dynamische plot stemkane

#check whether all receivers are in the raster
spatial <- loadSpatial("./spatial.csv")
sp_points <- terra::vect(
  spatial,
  geom = c("Longitude", "Latitude"),
  crs = terra::crs(base.raster_longlat)
)
check <- terra::extract(base.raster_longlat, sp_points)
#t.layer --> te groot om in te laden via load()

t.layer <- transitionLayer(base.raster_longlat, directions = 16)
#t.layer_lambert <- transitionLayer(base.raster_lamb, directions = 16)

######################################################################################################################
# run "runRSP"
# input = output of residency, migration of explore (actel package)
#runRSP_out_1 <- runRSP(explore_out, t.layer=t.layer, distance = 500, coord.x="Longitude", coord.y="Latitude",max.time = 6000, verbose =TRUE)
# problem: Out of memory for some tracks
runRSP_out_1 <- runRSP(
  explore_out,
  t.layer = t.layer,
  distance = 500,
  coord.x = "Longitude",
  coord.y = "Latitude",
  max.time = 6000,
  verbose = TRUE,
  tags = "A69-9006-3945"
)
#heb ik als output hiervan al regular tracks? (regularised by distance BUT only between 2 receivers)
#returns list of RSP tracks for each transmitter detected
#high max.time --> time between 2 detections from which a new track is created (this because uncertainty increases with time between detections)

#save(runRSP_out_1, file = "./data/analysis/runRSP_out_A69-9006-3945.RData")
load("./data/analysis/runRSP_out_A69-9006-3945.RData")

plotTracks(runRSP_out, base.raster = base.raster_longlat, tag = "A69-9006-3945")
animateTracks(
  runRSP_out,
  base.raster = base.raster_longlat,
  tags = "A69-9006-3945"
) #KOST HEEL VEEL TIJD
#ggsave("./figures/RSP/A69_9006_3945_plot.png")

###################################################################################################################
# dynBBMM
dBBMM <- dynBBMM(
  input = runRSP_out_1,
  base.raster = base.raster_longlat,
  UTM = 31
)
#run een hele namiddag en nacht lang :/

###################################################################################################################
# kijken of output van runRSP_out_1 kan gebruikt worden voor smoothing!
#zie smoothing.R

######################################################################################################
#debug
detections <- explore_out$valid.detections
spatial <- explore_out$spatial
tz <- explore_out$rsp.info$tz
#loadShape(): actel package
min.time <- 1
max.time <- 600
distance <- 250
er.ad <- 0.05
tz <- "UTC"
recaptures <- FALSE
transition <- t.layer
path.list <- list()
time.step <- 0.5
detections <- RSP:::prepareDetections(
  detections = explore_out$valid.detections,
  spatial = explore_out$spatial,
  coord.x = "Longitude",
  coord.y = "Latitude"
)
#RSP.time <- system.time(recipient <- RSP:::includeRSP(detections = detections, transition = t.layer,verbose=TRUE,recaptures=FALSE,
#time.step=0.5,min.time = 1, max.time = 600, distance=250, er.ad=0.05,tz="UTC"))

recipient <- RSP:::nameTracks(
  detections = detections[[1]],
  max.time = max.time,
  recaptures = recaptures,
  tz = tz
)
track.aux <- split(recipient$detections, recipient$detections$Track)
df.track <- track.aux[[1]]

#station.shifts <- c(FALSE, df.track$Standard.name[-1] != df.track$Standard.name[-nrow(df.track)])
#time.shifts <- df.track$Time.lapse.min > min.time
#same.station.shift <- !station.shifts & time.shifts

#df.track$Time.lapse.min <- c(0, as.numeric(difftime(df.track$Timestamp[-1], df.track$Timestamp[-nrow(df.track)], units = "mins")))
#function.recipient <- RSP:::calcRSP(df.track = df.track, tz = tz, distance = distance, verbose = TRUE, min.time = min.time,
#                                    time.step = time.step, transition = transition, er.ad = er.ad, path.list = path.list)
i <- 3
A <- with(df.track, c(Longitude[i - 1], Latitude[i - 1])) #gn-13
B <- with(df.track, c(Longitude[i], Latitude[i])) #gn-11 AANPASSIGEN IN LIJN 75 EN 76 STROMEN NIET DOOR TOT HIER????
test <- gdistance::shortestPath(transition, A, B, output = "SpatialLines")
p1 <- lines(test, col = "red", lwd = 2)
#error in gdistance::shortestPath! (graph.adjacency works), waarschijnlijk in igraph::get.shortest.paths
