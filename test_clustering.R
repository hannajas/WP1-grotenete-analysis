library(tidyverse)
library(dplyr)
library(sp)
library(sf)
library(adehabitatLT)
library(factoextra)

##############################################################################################################
# Load data
##############################################################################################################
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)


############################################
# With interpolated data - all eels combined
############################################
#cord.dec <- SpatialPoints(cbind(data$deploy_latitude, data$deploy_longitude),proj4string=CRS("+proj=longlat"))
cord.dec <- SpatialPoints(data[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
data[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)
resolution_s <- "5 min"
minutes <- 5

data$middledate <- as.POSIXct(data$arrival+data$residence/2)#PJ werkt op arrival time en niet op middledate
data$rounded_date <- floor_date(data$middledate, unit = resolution_s)
data$tag_serial_number <- as.character(data$tag_serial_number)

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
#t is one hour in seconds

#data$cluster <- NA

inter_all <- redisltraj(traj, u = 5*60, type = "time")
# id toevoegen aan elk traject
for (k in 1:length(inter_all)) {
    inter_all[[k]]$id <- id_unique[k]
}
data_inter <- do.call(rbind.data.frame, inter_all)# %>% filter(!is.na(dist))
data_inter$is_original <- NA
valid_ids <- unique(data_inter$id)
data_inter.eel <- split(data_inter, data_inter$id)

#density plot of dist
ggplot(data_inter, aes(x = dist/(5*60))) +
  geom_density(alpha = 0.6,line, fill = "#9ace9a") +#log on x
  scale_x_log10() +
  labs(
    title = "Density of Distances in Interpolated Data",
    x = "logspeed (m/s)",
  ) +
  theme_minimal()

g <- ggplot()
g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
g <- g + geom_density(aes(x =  dist/(5*60)), data = data_inter, fill="#69b3a2", color="#e9ecef", alpha=0.8)
g <- g + scale_x_log10(guide = "axis_logticks")
g <- g + geom_vline(xintercept=0.01, linetype = "dotted", linewidth = 1)
print(g)
#save
ggsave("./figures/Velocity/speed_m_s_distribution_interpolated_data.png")

# original data - Filter on eels with more that 1 data point
data_filtered <- data[data$tag_serial_number %in% valid_ids, ]
data.eel.filtered <- split(data_filtered, data_filtered$tag_serial_number)


for (k in seq_along(data_inter.eel)) {
  inter.temp <- data_inter.eel[[k]]
  data.temp <- data.eel.filtered[[k]]
  
  original_times <- data.temp$middledate
  new_times <- inter.temp$date
  
  #is_original <- floor_date(new_times, unit = resolution_s) %in% floor_date(original_times, unit = resolution_s) # check if the date in inter.temp is in the original data
  is_original <- new_times %in% original_times

  row_ids <- which(data_inter$id == names(data_inter.eel)[k])#select the row id's in data_inter of id k
  
  data_inter$is_original[row_ids] <- as.integer(is_original)
}

# with log but nog recommended, because now skewed to lower speeds
log_dist <- data_inter$dist
not_na_idx <- which(!is.na(log_dist))

not_na_idx <- which(!is.na(data_inter$dist))

# Run kmeans
result_log <- kmeans(data_inter$dist[not_na_idx], centers = 2, nstart = 10)
#esult <- kmeans(data$speed_m_s[not_na_idx], centers = 2, nstart = 10)
cluster_full <- rep(NA, length(data_inter$dist))
cluster_full[not_na_idx] <- result_log$cluster

data_inter$cluster <- as.factor(cluster_full)

data_cluster <- left_join(data, data_inter[, c("id", "date", "cluster", "is_original")], 
          by = c("tag_serial_number" = "id", "rounded_date" = "date"))

data_cluster_eel <- split(data_cluster, data_cluster$tag_serial_number)



pdf("./figures/Clustering/kmeans_interpolated_all_eels.pdf")
for (i in 1:length(data_cluster_eel)){
    mydfnew.temp <- data_cluster_eel[[i]]
    g <- ggplot(mydfnew.temp)
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = cluster), shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("red","green", "grey"))
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    #g <- g + labs(title = mydfnew.temp$tag_serial_number[1], subtitle = mydfnew.temp$catch_year) 
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

ggplot(data_inter, aes(x = dist, color = cluster)) +
  geom_density(linewidth = 1.2) +
  labs(
    title = "KMeans Clustering of Rediscretized Points",
    x = "speed (m/s)",
    color = "Cluster"
  ) +
  theme_minimal()

#How is the deviation for the speed in the original dataset?
ggplot(data_cluster, aes(x = speed_m_s, color = cluster)) +
  geom_density(linewidth = 1.2) +
  labs(
    title = "KMeans Clustering of Rediscretized Points",
    x = "speed (m/s)",
    color = "Cluster"
  ) +
  theme_minimal()


######################################################################################################
# plot clusters
data_filter <- data %>% filter(tag_serial_number == "1294161" & !is.na(speed_m_s))
#result <- Ckmeans.1d.dp(data_filter$speed_m_s, 2, method="linear")
result <- kmeans(log(data_filter$speed_m_s), centers = 2, nstart = 10)
plot(result)
data_filter$cluster_id <- factor(result$cluster)
ggplot(data_filter, aes(arrival, speed_m_s, color = cluster_id)) + 
  geom_point()


one_traj <- redisltraj(traj[16], u = 60*60*60, type = "time")
mydfnew.temp <- do.call(rbind.data.frame, one_traj)
k <- 2
result <- Ckmeans.1d.dp(mydfnew.temp$dist, k=2)
plot(result)



##############################################################################################################
# dbscan clustering
##############################################################################################################
library(dbscan)
library(factoextra)
speed <- data %>% filter(!is.na(speed_m_s) & tag_serial_number == "1294161") %>%
  select(arrival, speed_m_s) %>%
  mutate(speed_m_s = log(speed_m_s))
speed$speed_m_s
kNNdist <- kNNdistplot(as.matrix(speed$speed_m_s), k = 4)#epsilon = 0.015

db <- dbscan(as.matrix(speed$speed_m_s), eps = 0.04, minPts = 2)
print(db)
#8 clusters AND 5 noise points (for all eels together!)







############################################
# With interpolated data - all eels seperatly
############################################
mydfnew.split.eel <- split(data, data$tag_serial_number)

pdf("./figures/Clustering/kmeans_log_interpolated_sep_eels.pdf")
for(a in 1:length(traj)){
  mydfnew.temp <- mydfnew.split.eel[[i]] %>% filter(!is.na(speed_m_s))
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 5*60, type = "time")
    mydfnew.temp <- do.call(rbind.data.frame, one_traj) %>% filter(!is.na(dist))
    if (nrow(mydfnew.temp) < 2) {
        next
    }
    #result <- Ckmeans.1d.dp(mydfnew.temp$dist, k)
    result <- kmeans(log(mydfnew.temp$dist), centers = 2, nstart = 10)
    centers <- result$centers
    cluster_map <- if (centers[1] < centers[2]) c(1, 2) else c(2, 1)
    reassigned_clusters <- cluster_map[result$cluster]
    
    mydfnew.temp$cluster <- as.factor(reassigned_clusters)
    
    # Ensure cluster column aligns with the original data
    mydfnew.split.eel[[i]]$cluster <- NA
    mydfnew.split.eel[[i]]$cluster[!is.na(mydfnew.split.eel[[i]]$speed_m_s)] <- mydfnew.temp$cluster


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