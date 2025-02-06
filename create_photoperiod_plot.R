library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)


# Upload dataset
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$tag_serial_number <- factor(data$tag_serial_number)
data$migration <- factor(data$migration)
data$station_name <- factor(data$station_name)

env_data <- read_csv('./data/raw/photoperiod_verwerkt.csv')
env_data$photoperiod <- as.numeric(hms(env_data$photoperiod),"minutes")
env_data$date <- ymd(env_data$date,tz="UTC")






# Create pdf with distance tracks
mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs
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
    #g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (240*60*60), y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
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