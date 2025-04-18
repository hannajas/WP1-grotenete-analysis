# Create plots with travelled distance and store as .pdf
# by Ine Pauwels, Pieterjan Verhelst & Stijn Bruneel
# ine.pauwels@inbo.be, pieterjan.verhelst@inbo.be, stijn.bruneel@inbo.be

library(tidyverse)
library(lubridate)
library(tidyquant)


# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$tag_serial_number <- factor(data$tag_serial_number)
data$migration <- factor(data$migration)
data$station_name <- factor(data$station_name)


# Create plot for all eels in single pdf
#data$arrival <- ymd_hms(data$arrival)
#data$arrival <- as.POSIXct(strptime(data$arrival,"%Y-%m-%d %H:%M:%S"))

min(data$distance_to_source_m)  # Identify the min limit for the y-axis
max(data$distance_to_source_m)  # Identify the max limit for the y-axis


# Select eels considered migratory
# 'has_migration_started' == TRUE
#data <- filter(data, has_migration_started == "TRUE")

# In case a movement range is used.
# Here we only select eels that had a movement range of minimum 4000 m
#migrants <- data %>%
#  select(acoustic_tag_id, distance_to_source_m) %>%
#  group_by(acoustic_tag_id) %>%
#  mutate(total_distance = max(distance_to_source_m)-min(distance_to_source_m)) %>%
#  select(-distance_to_source_m) %>%
#  distinct()

#migrants <- filter(migrants, total_distance > 4000)

#data<- subset(data, acoustic_tag_id %in% migrants$acoustic_tag_id)
#data$acoustic_tag_id <- factor(data$acoustic_tag_id)


# Create pdf with distance tracks
mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs
pdf("./figures/2019_grotenete_migration_4000m_0.01ms.pdf") # Create pdf


for (i in 1:length(mydfnew.split.eel)){ #i van 1 tot aantal transmitters
  mydfnew.temp<-mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
  g <- ggplot()
  g <- g + theme(axis.text.x = element_text(size = 12, colour = "black", angle=90))
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
  g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
  g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
  g <- g + scale_color_manual(values = c("FALSE" = "red",
                                         "TRUE" =  "green"))
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
  #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
  g <- g + labs(title = mydfnew.temp$tag_serial_number, subtitle = mydfnew.temp$catch_year) 
  g <- g + ylab("Distance (m)")
  g <- g + xlab("Date")
  g <- g + scale_x_datetime(date_breaks  ="1 week")
  g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed")
  g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (240*60*60), y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
  g <- g + theme(legend.position="bottom")
  print(g)
}

dev.off()






## Create single plot

# Select individual
data2 <- data[which(data$tag_serial_number == "1171748"), ]
#data2=data2[order(as.POSIXct(strptime(data2$Arrival,"%d/%m/%Y %H:%M"))),]
data2 <- data2[order(as.POSIXct(strptime(data2$arrival,"%Y-%m-%d %H:%M:%S"))),]


# Create plot
ggplot() + geom_line(aes(arrival, -1*distance_to_source_m/1000), data = data2, colour = "black", size = 1) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = migration), data = data2, shape = 16, size = 5) +
  scale_color_manual(values = c("FALSE" = "red",
                                "TRUE" =  "green")) +
  ggtitle(data2$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#  scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  scale_x_datetime(date_breaks  ="1 month") + 
  geom_hline(yintercept = -1*data2$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")




