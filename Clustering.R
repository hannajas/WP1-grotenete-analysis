library(tidyverse)
library(dplyr)
library(Ckmeans.1d.dp)
library(sp)
library(sf)
library(adehabitatLT)


data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
k <- 2

############################################
# With raw data
mydfnew.split.eel <- split(data, data$tag_serial_number)
pdf("./figures/2019_grotenete_migration_1D_clustering_manhattan.pdf")
for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]]
    #distances <- dist(mydfnew.temp$speed_m_s, method = "manhattan")
    result <- Ckmeans.1d.dp(mydfnew.temp$speed_m_s, k)
    mydfnew.temp$cluster <- as.factor(result$cluster)
    #mydfnew.temp$cluster <- result$cluster #zogezegd 210 rijen???
    g <- ggplot(mydfnew.temp)
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = cluster), shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("red","green"))
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    g <- g + labs(title = mydfnew.temp$tag_serial_number[1], subtitle = mydfnew.temp$catch_year) 
    g <- g + ylab("Distance (m)")
    g <- g + xlab("Date")
    # g <- g + scale_x_date(date_labels = "%b-%d-%Y")
    g <- g + scale_x_datetime(date_breaks  ="1 week")
    g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", linewidth = 0.5, linetype = "dashed")
    g <- g + annotate("text",x = mydfnew.temp$arrival[1], y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
    g <- g + theme(legend.position="bottom")
    print(g)
}
dev.off()



############################################
# with interpolated data
cord.dec <- SpatialPoints(cbind(data$deploy_latitude, data$deploy_longitude),proj4string=CRS("+proj=longlat"))
cord.dec <- SpatialPoints(data[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
data[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)

data$middledate <- as.POSIXct(round_date(data$arrival+data$residence/2, "15 mins"))#PJ werkt op arrival time en niet op middledate
id <- unique(data$tag_serial_number)
#Store data in an object of class "ltraj"
data <- data %>%#BUG IN THE ADEHABITAT R PACKAGE! these columns should be named x and y
  rename(
    x = deploy_longitude,
    y = deploy_latitude
    )
xy <- data[,c("x", "y")]
date <- data$middledate
id <- as.character(data$tag_serial_number)
id_unique <- unique(id)
traj <- as.ltraj(xy,data$middledate, id) #traj[[1]]$dist --> 21 object for which the last one is NA

pdf("./figures/2019_grotenete_migration_1D_clustering_interpolated.pdf")
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*60*60, type = "time")
    mydfnew.temp <- do.call(rbind.data.frame, one_traj)
    result <- Ckmeans.1d.dp(mydfnew.temp$dist, k)
    mydfnew.temp$cluster <- as.factor(result$cluster)
    mydfnew.temp$cum_dist <- cumsum(mydfnew.temp$dist)
    g <- ggplot(mydfnew.temp)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14))+
    geom_line(aes(date, cum_dist, color = cluster), linewidth = 1)+
    scale_color_manual(values = c("red","green"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()



######################################################################################################
# plot clusters
data_filter <- data %>% filter(tag_serial_number == "1294161")
result <- Ckmeans.1d.dp(data_filter$speed_m_s, 2, method="linear")
plot(result)



one_traj <- redisltraj(traj[16], u = 60*60*60, type = "time")
mydfnew.temp <- do.call(rbind.data.frame, one_traj)
k <- 2
result <- Ckmeans.1d.dp(mydfnew.temp$dist, k=2)
plot(result)