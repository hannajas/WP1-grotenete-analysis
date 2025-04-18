library("remotes")
library("actel")
library(tidyverse)
library(RSP)
library(dplyr)
library(sp)

#Data
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
detections <- read_csv('./data/raw/raw_detection_data.csv', show_col_types = FALSE) %>%#fiter out acoustic project code = "bpns", CONNECT-MED, Orstedcod, SPAWNSEIS,cpodnetwork,OP-Test
    filter(acoustic_project_code != "bpns") %>%
    filter(acoustic_project_code != "CONNECT-MED") %>%
    filter(acoustic_project_code != "Orstedcod") %>%
    filter(acoustic_project_code != "SPAWNSEIS") %>%
    filter(acoustic_project_code != "cpodnetwork") %>%
    filter(acoustic_project_code != "OP-Test") %>%
    filter(acoustic_project_code != "MOBEIA")
detections$Receiver <- unlist(map(strsplit(detections$receiver_id, "-"), 2))


###########################################################################
# distance matix
#load distance matrix
distance_matrix <- read_csv('./data/raw/distancematrix_2019_grotenete.csv', show_col_types = FALSE)
#rename ..1 row id's to Standard.Name in spatial_actel


#get receiver ID's
stations <- unique(detections$station_name)
receiver_id <- c()
for (i in 1:length(stations)) {
    filter_det <- detections[detections$station_name ==stations[i],]
    link <- unique(filter_det$Receiver)
    if (length(link) > 1) {
        print(paste("stations:", stations[i],"receivers:",link))
        receiver_id <- c(receiver_id,link[1])#else
        detections$Receiver[detections$station_name == stations[i]] <- link[1]
    } else {
       receiver_id <- c(receiver_id,link)
    }
}

##########################################################################
#biometry
eel_meta_data <- read_csv('./data/raw/eel_meta_data.csv', show_col_types = FALSE)
biometry <- eel_meta_data %>%
  rename(Signal = acoustic_tag_id,
  Release.date = release_date_time,
  Release.site = release_location,
  Serial.nr = tag_serial_number) %>%
  mutate(Release.date = as.POSIXct(dmy_hm(Release.date), format = "%Y-%m-%d %H:%M", tz = "UTC"),
  Signal = extractSignals(eel_meta_data$acoustic_tag_id),
  Code.space = extractCodeSpaces(eel_meta_data$acoustic_tag_id))
#esle if struvture if strslit gives Na KEEP THE PREVIOUS OTHERWISE DO THE SPLIT
biometry$Code.space[grepl(",", biometry$Code.space)] <- unlist(map(strsplit(unique(biometry$Code.space), ","), 2))
#replace names in biometry$Release.site with other names
biometry$Release.site <- gsub('Downstream confluent Grote Nete and Molse Nete', 'rel_grotenete1',
gsub('Grote Nete 200 m upstream deployment gn-14','rel_grotenete2',
gsub('Wildersedijk Grote Nete','rel_grotenete3', biometry$Release.site)))
#write_csv(biometry, './biometrics.csv')

###########################################################################
# spatial 
deployments <- read_csv('./data/raw/deployments.csv', show_col_types = FALSE) %>%
    filter(station_name %in% stations) %>%
    rename(Latitude = deploy_latitude,
           Longitude = deploy_longitude,
           Station.name = station_name) %>%
           select(-c("...1"))
deployments$Type <- "Hydrophone"
#runRSP(coord.x=deploy_longitude, coord.y=deploy_latitude)

release <- data.frame(
  acoustic_project_code = c("2019_Grotenete","2019_Grotenete","2019_Grotenete"),
  Station.name = c("rel_grotenete1", "rel_grotenete2", "rel_grotenete3"),
  Latitude = c(51.144779, 51.139997, 51.138881),
  Longitude = c(5.003906, 4.997392, 4.996931),
  Type = c("Release", "Release", "Release")
)

spatial <- rbind(deployments, release)
spatial$Array <- "A1"
spatial$Section <- "River"
spatial <- left_join(distance_matrix,spatial, by=c("...1"="Station.name")) %>%
    select(-all_of(c(stations,"rel_grotenete1","rel_grotenete2","rel_grotenete3"))) %>%
    rename(Station.name = ...1)
write_csv(spatial, './spatial.csv')

#########################################################################
# deployments
deployments <- data.frame(Station.name= distance_matrix$...1)%>%
filter(!grepl("rel",Station.name))
deployments$Receiver <- NA
deployments$Start <- min(biometry$Release.date)
deployments$Stop <- max(biometry$Release.date)+days(85)
for (i in 1:length(deployments$Station.name)) {
    ind <- which(stations == deployments$Station.name[i])
    print(paste(i))
    deployments$Receiver[i] <- receiver_id[ind]
}
#write_csv(deployments, './deployments.csv')



#########################################################################
#rename the distance matrix
spatial_standard_name <- loadSpatial()
distance_matrix$...1 <- spatial_standard_name$Standard.name
#rename the columns of the distance matrix as spatial_standard_name$Standard.name:
colnames(distance_matrix)[-1] <- spatial_standard_name$Standard.name[match(
  colnames(distance_matrix)[-1], spatial_standard_name$Station.name
)]
#write_csv(distance_matrix, './distances.csv')



#########################################################################
# detections
#the detections data needs other column names
movements <- detections %>%
    rename(Timestamp = date_time)
movements$CodeSpace <- extractCodeSpaces(movements$acoustic_tag_id)
movements$Signal <- extractSignals(movements$acoustic_tag_id)
#write_csv(movements, './detections/detections.csv')




#create an actel object
explore_out <- explore(tz="UTC",GUI="never")
# max time mag heel hoog --> we willen niet dat hij een nieuwe track begint


###########################################################################
# RSP
# load t.layer
# add Lambert coordiates to spatial

cord.dec <- SpatialPoints(spatial[,c("Longitude","Latitude")],proj4string=CRS("+proj=longlat"))
lambert <- spTransform(cord.dec,CRS("EPSG:31370"))
spatial[,c("X","Y")] <- coordinates(lambert)
DEM <- raster("C:/Users/hjaspaer/OneDrive - UGent/Documents/Werkpakkket I/GIS/raster_water_grote_nete.tif")
study_area <- terra::rast("C:/Users/hjaspaer/OneDrive - UGent/Documents/Werkpakkket I/GIS/raster_water_grote_nete.tif")
base.raster <- shapeToRaster("./data/raw/shape/Grote_nete_water.shp", size =100, coord.x = "X", coord.y="Y", type = "water")
raster::plot(study_area, col ="blue")#niets miss met .tif file R plot het meteen naar hij verlaagt de resolutie
#resolutie vehogen miss in raster package proberen --> DPI verhoger
#hv plot plotly in python dynaische plot ste mkane
t.layer <- transitionLayer(study_area, directions = 16)
# run "runRSP" 
# input = output of residency, migration of explore (actel package)
test <- runRSP(explore_out, coord.x="Longitude", coord.y="Latitude")