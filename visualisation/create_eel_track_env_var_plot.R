library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)
library(adehabitatLT)
library(sp)
library(sf)


# Upload dataset
data <- read_csv('./data/interim/migration_env_filter.csv') 
metadata <- read_csv('./data/interim/Metadata.csv', show_col_types = FALSE)
############################################################################################################




#env_data <- read_csv('./data/raw/photoperiod_verwerkt.csv')
#env_data$photoperiod <- as.numeric(hms(env_data$photoperiod),"minutes")
#env_data$date <- ymd(env_data$date,tz="UTC")


# Start plotting
mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs

# plot all eel trajectories on one plot
all_color <- c("red","blue", "green", "purple", "orange", "black", "yellow", "pink", "brown", "grey", "cyan", "magenta", "darkgreen", "darkred", "darkblue", "darkorange","darkgrey","darkcyan", "darkmagenta","red","blue", "green", "purple", "orange", "black", "yellow", "pink", "brown", "grey", "cyan","darkorange","red","blue", "green", "purple", "orange", "black", "yellow", "pink")
min_date <- min(data$arrival)
max_date <- max(data$departure)
g <- ggplot()
g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
g <- g + geom_hline(yintercept = -1*data$distance_to_source_m, colour = "gray", linewidth = 0.5, linetype = "dashed")
g <- g + annotate("text",x = data$arrival[1], y = -1*data$distance_to_source_m, label = data$station_name, hjust=0, colour="red", size = 3)
for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = all_color[i], linewidth = 1)
    #g <- g + geom_point(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, shape = 16, size = 5)
}
g <- g + xlim(min_date, max_date)



###########################################################################################################
# Plot eel trajectories with color based on downstream_migration
pdf("./figures/2019_grotenete_migration_speed_m_s_0.01.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = downstream_migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
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


##########################################################################################################
# Plot the Tw together with the eel trajectories
pdf("./figures/2019_grotenete_migration_Tw.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
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

    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_point(aes(date, Tw),data = mydfnew.temp, colour = "black",linewidth=1)
    g2 <- g2 + geom_line(aes(date, Tw),data = mydfnew.temp, colour = "grey",linewidth=0.5, linetype ="dashed")
    g2 <- g2 + ylab("T (°C)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()

##########################################################################################################
# Plot the V_w together with the eel trajectories
pdf("./figures/Trajectory_linked_env_var/2019_grotenete_migration_Vw.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
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

    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_point(aes(date, V),data = mydfnew.temp, colour = "black",linewidth=1)
    g2 <- g2 + geom_line(aes(date, V),data = mydfnew.temp, colour = "grey",linewidth=0.5, linetype ="dashed")
    g2 <- g2 + ylab("V (m/s)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()


##########################################################################################################
# Plot the Q together with the eel trajectories
pdf("./figures/2019_grotenete_migration_Q.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
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

    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_point(aes(date, delta_Q),data = mydfnew.temp, colour = "black",linewidth=1)
    g2 <- g2 + geom_line(aes(date, delta_Q),data = mydfnew.temp, colour = "grey",linewidth=0.5, linetype ="dashed")
    g2 <- g2 + ylab("T (°C)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()

###########################################################################################################
# Create pdf with distance tracks
pdf("./figures/2019_grotenete_migration_photoperiod.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    #range of times of detection
    min_date <- which(env_data$date==min(mydfnew.temp$date, na.rm=TRUE))
    max_date <- which(env_data$date==max(mydfnew.temp$date, na.rm=TRUE))

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    g <- g + labs(title = mydfnew.temp$tag_serial_number[min_date], subtitle = mydfnew.temp$catch_year) 
    g <- g + ylab("Distance (m)")
    g <- g + xlab("Date")
    # g <- g + scale_x_date(date_labels = "%b-%d-%Y")
    g <- g + scale_x_datetime(date_breaks  ="1 week")
    g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", linewidth = 0.5, linetype = "dashed")
    g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (240*60*60), y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
    g <- g + theme(legend.position="bottom")

    mydfenv.temp <- env_data[min_date:max_date,1:ncol(env_data)]
    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_line(aes(date, photoperiod),data = mydfenv.temp, colour = "black",linewidth=1)
    g2 <- g2 + ylab("Photoperiod (min)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()



##########################################################################################################
# Plot the R together with the eel trajectories
pdf("./figures/2019_grotenete_migration_R.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    mydfnew.temp$date <- round_date(mydfnew.temp$arrival + (mydfnew.temp$departure - mydfnew.temp$arrival) / 2,unit="day")

    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
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

    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_col(aes(date, R),data = mydfnew.temp, colour = "black",width=0.01)
    g2 <- g2 + geom_point(aes(date, R),data = mydfnew.temp, colour = "black",size=1)
    #g2 <- g2 + geom_line(aes(date, R),data = mydfnew.temp, colour = "grey",linewidth=0.5, linetype ="dashed")
    g2 <- g2 + ylab("R (mm)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()



###########################################################################################################
# PDF with Q of one location
# migration data in the right format for adehabitat package
L10_077_Q <- read_csv('./data/raw/discharge/L10_077_Q.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
#longitude, latitude to UTM
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

pdf("./figures/2019_grotenete_migration_Q_L10_077.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")
    temp_traj <- do.call(rbind.data.frame, one_traj) %>%
        left_join(L10_077_Q,"date")
    temp_traj$cum_dist <- cumsum(temp_traj$dist)
    g <- ggplot(temp_traj)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cum_dist), linewidth = 1)+
    geom_line(aes(date, Value*22222), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /22222, name = "Discharge (m³/s)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()


###########################################################################################################
# PDF with R of one location
P10_011_R <- read_csv('./data/raw/rainfall/P10_011_R.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
pdf("./figures/2019_grotenete_migration_R_P10_011.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")
    temp_traj <- do.call(rbind.data.frame, one_traj) %>%
        left_join(P10_011_R,"date")
    temp_traj$cum_dist <- cumsum(temp_traj$dist)
    g <- ggplot(temp_traj)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cum_dist), linewidth = 1)+
    geom_line(aes(date, cumsum(Value)*1111),colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /1111, name = "Precipitation (mm)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()

###########################################################################################################
#Tw
L10_077_Tw <- read_csv('./data/raw/temperature/L07_077_Tw.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
pdf("./figures/2019_grotenete_migration_Tw_L07_077.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")
    temp_traj <- do.call(rbind.data.frame, one_traj) %>%
        left_join(L10_077_Tw,"date")
    temp_traj$cum_dist <- cumsum(temp_traj$dist)
    g <- ggplot(temp_traj)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cum_dist), linewidth = 1)+
    geom_line(aes(date, Value*5555), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /5555, name = "Temperature (°C)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()

###########################################################################################################
# PDF with turbidity
rup02e_SF_1066_turb <- read_csv('./data/raw/turbidity/rup02e_SF_1066_turb.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )

pdf("./figures/2019_grotenete_migration_rup02e_SF_1066_turb.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*5, type = "time")
    temp_traj <- do.call(rbind.data.frame, one_traj) %>%
        left_join(rup02e_SF_1066_turb,"date")
    temp_traj$cum_dist <- cumsum(temp_traj$dist)
    g <- ggplot(temp_traj)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cum_dist), linewidth = 1)+
    geom_line(aes(date, Value*555), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /555, name = "Turbidity (NTU)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()

###########################################################################################################
# PDF with O_diss

rup02e_SF_1066_O <- read_csv('./data/raw/oxygen/rup02e_SF_1066_O.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )

pdf("./figures/2019_grotenete_migration_rup02e_SF_1066_O.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*5, type = "time")
    temp_traj <- do.call(rbind.data.frame, one_traj) %>%
        left_join(rup02e_SF_1066_O,"date")
    temp_traj$cum_dist <- cumsum(temp_traj$dist)
    g <- ggplot(temp_traj)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cum_dist), linewidth = 1)+
    geom_line(aes(date, Value*5555), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /5555, name = "Turbidity (NTU)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    print(g)
    }
}

dev.off()


###########################################################################################################
# pdf with Q,R and Tw

pdf("./figures/2019_grotenete_migration_Q_R_Tw.pdf") # Create pdf
for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")
    temp_traj_1 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(L10_077_Q,"date")
    g1 <- ggplot(temp_traj_1)+
    theme(axis.text.x = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, Value*22222), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /22222, name = "Discharge (m³/s)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    temp_traj_2 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(L10_077_Tw,"date")
    g2 <- ggplot(temp_traj_2)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, Value*5555), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /5555, name = "Temperature (°C)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    ylab("Distance (m)")+
    xlab("Date")
    temp_traj_3 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(P10_011_R,"date")
    g3 <- ggplot(temp_traj_3)+
    theme(axis.text.x = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, cumsum(Value*1111)), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /1111, name = "Precipitation (mm)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    ylab("Distance (m)")+
    xlab("Date")
    print(g1 / g3 / g2)
    }
}

dev.off()


###########################################################################################################
# pdf with turbidity and oxygen, salinity and turbidity
# departing from the tidal zones! (rupel)
# turbidity and oxygen zouden ook een verklaring kunnen zijn voor de initiele start van migratie MAAR hiervoor niet genoeg opwaarste data! om dit te testen
rup02e_SF_1066_O <- read_csv('./data/interim/processed/rup02e_SF_1066_O.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
rup02e_SF_1066_turb <- read_csv('./data/interim/processed/rup02e_SF_1066_turb.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
rup02e_SF_1066_S <- read_csv('./data/interim/processed/rup02e_SF_1066_S.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
rup00a_1066_Q <- read_csv('./data/interim/processed/rup00a_1066_Q.csv', show_col_types = FALSE) %>%
    rename(
        date = Timestamp
    )
data <- read_csv('./data/interim/migration_env_filter.csv') 

#longitude, latitude to UTM
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
data_filter <- data %>%
    filter(zone == "tidal")
xy <- data_filter[,c("x", "y")]
date <- data_filter$middledate
id <- as.character(data_filter$tag_serial_number)
id_unique <- unique(id)
traj <- as.ltraj(xy,data_filter$middledate, id) #traj[[1]]$dist --> 21 object for which the last one is NA


pdf("./figures/2019_grotenete_migration_O_S_T.pdf") # Create pdf

for(a in 1:length(traj)){
  if (nrow(traj[[a]]) > 1) {
    one_traj <- redisltraj(traj[a], u = 60*15, type = "time")
    temp_traj_1 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(rup02e_SF_1066_turb,"date")
    g1 <- ggplot(temp_traj_1)+
    theme(axis.text.x = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, Value*300), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /300, name = "Turbidity (NTU)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    labs(title = id_unique[a])+
    ylab("Distance (m)")+
    xlab("Date")
    temp_traj_2 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(rup02e_SF_1066_O,"date")
    g2 <- ggplot(temp_traj_2)+
    theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, Value*4000), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /4000, name = "Oxygen (mg/l)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    ylab("Distance (m)")+
    xlab("Date")
    temp_traj_3 <- do.call(rbind.data.frame, one_traj) %>%
        left_join(rup02e_SF_1066_S,"date")
    g3 <- ggplot(temp_traj_3)+
    theme(axis.text.x = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_text(size=16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color="blue"))+
    geom_line(aes(date, cumsum(dist)), linewidth = 1)+
    geom_line(aes(date, Value*40000), colour = "blue", linewidth = 1)+
    scale_y_continuous(sec.axis = sec_axis(~. /40000, name = "Salinity (spu)"))+
    theme(plot.title = element_text(lineheight=.8, face="bold", size=20))+
    ylab("Distance (m)")+
    xlab("Date")
    print(g1 / g3 / g2)
    }
}

dev.off()

########################################################################################################
#Q

pdf("./figures/Trajectory/2019_tidal_migration_Q.pdf") # Create pdf

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    g <- g + labs(title = mydfnew.temp$tag_serial_number[1], subtitle = mydfnew.temp$catch_year) 
    g <- g + ylab("Distance (m)")
    g <- g + xlab("Date")
    g <- g + scale_x_datetime(date_breaks  ="1 week")
    g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", linewidth = 0.5, linetype = "dashed")
    g <- g + annotate("text",x = mydfnew.temp$arrival[1], y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
    g <- g + theme(legend.position="bottom")
    min <- min(mydfnew.temp$arrival)
    max <- max(mydfnew.temp$arrival)
    env_data <- rup00a_1066_Q[rup00a_1066_Q$date >= min & rup00a_1066_Q$date <= max,]
    g2 <- ggplot()+
    geom_point(aes(date, Value),data = env_data, colour = "black",linewidth=1)
    print(g2 / g + plot_layout(heights = c(1,2)))
}

dev.off()

###########################################################################################################
#O

mydfnew.split.eel <- split(data_filter, data_filter$tag_serial_number) # split dataset based on tag IDs

for (i in 1:length(mydfnew.split.eel)){
    mydfnew.temp <- mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
    g <- ggplot()
    g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
    g <- g + geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + scale_color_manual(values = c("FALSE" = "red",
                                            "TRUE" =  "green"))
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    g <- g + labs(title = mydfnew.temp$tag_serial_number[1], subtitle = mydfnew.temp$catch_year) 
    g <- g + ylab("Distance (m)")
    g <- g + xlab("Date")
    g <- g + scale_x_datetime(date_breaks  ="1 week")
    g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", linewidth = 0.5, linetype = "dashed")
    g <- g + annotate("text",x = mydfnew.temp$arrival[1], y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
    g <- g + theme(legend.position="bottom")

    g2 <- ggplot()
    g2 <- g2 + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
    g2 <- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))
    g2 <- g2 + geom_point(aes(arrival, O),data = mydfnew.temp, colour = "black",linewidth=1)
    g2 <- g2 + geom_line(aes(arrival, O),data = mydfnew.temp, colour = "grey",linewidth=0.5, linetype ="dashed")
    g2 <- g2 + ylab("O (mg/l)")
    g2 <- g2 + xlab("Date")
    g2 <- g2 + scale_x_datetime(date_breaks  ="1 week")
    g2 <- g2 + theme(legend.position = "none")
    print(g2 / g + plot_layout(heights = c(1,2)))  
}
dev.off()
