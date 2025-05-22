library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)
library(geosphere)
library(adehabitatLT)
library(sp)
library(sf)

# source functions
source('./src/allocate_segments_function.R')

# migration data in the right format for adehabitat package
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)

#longitude, latitude to UTM
cord.dec <- SpatialPoints(cbind(data$deploy_latitude, data$deploy_longitude),proj4string=CRS("+proj=longlat"))
cord.dec <- SpatialPoints(data[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
data[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)
#distance wordt hier in vogelvlucht berekend MAAR paling gaat langs de rivier
# IK HEB GEEN INFO OVER DE TALLIJN! DUS niet juist te berekenen op deze resolutie


############################################################################################
# PRE-PROCESS TRAJECTORY
# irrerular trajectory
data$middledate <- as.POSIXct(round_date(data$arrival+data$residence/2, "15 mins"))#PJ werkt op arrival time en niet op middledate

#Store data in an object of class "ltraj"
data <- data %>%#BUG IN THE ADEHABITAT R PACKAGE! these columns should be named x and y
  rename(
    x = deploy_longitude,
    y = deploy_latitude
    )
xy <- data[,c("x", "y")]
date <- data$middledate
id <- as.character(data$tag_serial_number)
traj <- as.ltraj(xy,data$middledate, id) #traj[[1]]$dist --> 21 object for which the last one is NA

# plot the trajectory
#traj[[8]]$index <- 1:length(traj[[8]]$date)
#p1 <- ggplot()+
#  geom_point(aes(index,dist),data = traj[[8]])# blauwe driehoek = start, rode driehoek = end



############################################################################################
# LAVIELLE ANALYSIS
data_list <- split(data, f = data$tag_serial_number)
pdf("./figures/2019_grotenete_migration_Lavielle_K=4_dist_meanvar.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")# INTERPOLATE to a regular trajectory
    #one_traj[[1]]$dist <- log(one_traj[[1]]$dist)
    lav <- lavielle(one_traj, Lmin=2, Kmax=8, type="meanvar")
    kk <- findpath(lav,4,plotit = TRUE)
    #data_temp <- data %>% filter(tag_serial_number == data_list[[a]]$tag_serial_number[1])
    temp_oud <- c()
    for(i in 1:length(kk)){#door de opdeling van segmenten worden er NA waarden gecreeerd bij de wissel van segmenten!
      #print(paste(i))
      kk[[i]]$date <- as.POSIXct(kk[[i]]$date, tz = "UTC")
      temp <- left_join(kk[[i]],data_list[[a]], by = c("date"="middledate"))
      temp$segment <- i
      temp <- rbind(temp_oud,temp[1:(length(temp$segment)-1),])
      temp_oud <<- temp
    }
    temp$cum_dist <- cumsum(temp$dist)

    p1 <- ggplot() + 
      geom_point(aes(date, cum_dist, colour = segment), data = temp, shape = 16, size = 7) + 
      #geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = segment), data = subset(temp, !is.na(distance_to_source_m)), shape = 16, size = 7)+ 
      ggtitle(data_list[[a]]$tag_serial_number[1]) +
      theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
      ylab("Distance (km)") +
      xlab("Date") + 
    #scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle=90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25))+
      scale_x_datetime(date_breaks  ="1 week") + 
      #geom_hline(yintercept = -1*data_1171749$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
      #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
      theme(legend.position="none")
    print(p1)
  }

}
dev.off()


############################################################################################
# LAVIELLE ANALYSIS - FOR ONE EEL
data_1171749 <- data %>% filter(tag_serial_number == "1171749")
#traj[[29]]$dist <- c(data_1171749$swimdistance_m[-1],NA)
#BINNEN traj[] juiste nummer van eel trajectory invullen!
one_traj <- redisltraj(traj[4], u = 60*15, type = "time")# hoedanook wordt voor deze interpolatie gebruikgemaakt van de originele dist!
#one_traj[[1]]$dist <- log(one_traj[[1]]$dist)

#p2 <- ggplot()+
#  geom_point(aes(date,log(dist)),data = traj[[16]])

lav <- lavielle(one_traj, Lmin=2, Kmax=8, type="var")#traj[25]
#the series used (lav$series) == traj[[1]] without the last (NA) value

test <- chooseseg(lav)#waarom zijn y-waarden negatief??
kk <- findpath(lav,2,plotit = TRUE)
#create empty dataframe 'temp_oud'

temp_oud <- c()
for(i in 1:length(kk)){#door de opdeling van segmenten worden er NA waarden gecreeerd bij de wissel van segmenten!
  #print(paste(i))
  kk[[i]]$date <- as.POSIXct(kk[[i]]$date, tz = "UTC")
  temp <- left_join(kk[[i]],data_1171749, by = c("date"="middledate"))
  temp$segment <- i
  temp <- rbind(temp_oud,temp[1:(length(temp$segment)-1),])
  temp_oud <<- temp
}
temp$cum_dist <- cumsum(temp$dist)
#data_1171749 <- allocate_segments(kk,data_1171749)


p1 <- ggplot() + 
  geom_point(aes(date, cum_dist, colour = segment), data = temp, shape = 16, size = 4) + 
  #geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = segment), data = subset(temp, !is.na(distance_to_source_m)), shape = 16, size = 7)+ 
  #ggtitle(data_1171749$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))+
  scale_x_datetime(date_breaks  ="1 week") + 
  #geom_hline(yintercept = -1*data_1171749$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")



p1 <- ggplot() + 
  geom_line(aes(arrival, -1*distance_to_source_m/1000), data = subset(temp, !is.na(distance_to_source_m)), colour = "black", size = 2) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = segment), data = subset(temp, !is.na(distance_to_source_m)), shape = 16, size = 7)+ 
  #ggtitle(data_1171749$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))+
  scale_x_datetime(date_breaks  ="1 week") + 
  geom_hline(yintercept = -1*data_1171749$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")




p1 <- ggplot() + 
  geom_line(aes(arrival, -1*distance_to_source_m/1000), data = subset(temp, !is.na(distance_to_source_m)), colour = "black", size = 2) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = segment), data = subset(temp, !is.na(distance_to_source_m)), shape = 16, size = 7)+ 
  #ggtitle(data_1171749$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))+
  scale_x_datetime(date_breaks  ="1 week") + 
  geom_hline(yintercept = -1*data_1171749$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")








############################################################################################
# GUEGUEN ANALYSIS - try for 1171749
data_1171749 <- data %>% filter(tag_serial_number == "1294169")
one_traj <- redisltraj(traj[23], u = 60*15, type = "time")
#traj[[4]]$dist <- data_1171749$migration_speed
plotltr(one_traj, "dist")
tested.means <- seq(0, 1.5, length = 10)
tested.sd <- seq(0.05, 0.1, length = 10)

limod <- as.list(paste("dnorm(dist, mean =",tested.means, ", sd = ",tested.sd,")"))
mod <- modpartltraj(one_traj, limod)
bestpartmod(mod)
pm <- partmod.ltraj(one_traj, 2, mod)

data_1171749 <- allocate_segments(pm$ltraj,data_1171749)

p1 <- ggplot() + geom_line(aes(arrival, -1*distance_to_source_m/1000), data = data_1171749, colour = "black", size = 2) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = segment), data = data_1171749, shape = 16, size = 7)+ 
  ggtitle(data_1171749$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))+
  scale_x_datetime(date_breaks  ="1 week") + 
  geom_hline(yintercept = -1*data_1171749$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")