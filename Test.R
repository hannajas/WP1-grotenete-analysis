# plot the speed of the fish for each segment
pdf("./figures/Downstream_segments_Tw_Q.pdf") # Create pdf
for (i in 2:length(seq_af)-1){
    print(paste("Processing segment:", seq_af[i], "to", seq_af[i + 1]))
    lag_station_name <- dplyr::lag(data_filter$station_name)
    print(paste("lag_station_name:", length(lag_station_name)))
    print(paste("station_name:", length(data_filter$station_name)))
    data_temp <- filter(data_filter, lag_station_name == seq_af[i] & station_name == seq_af[i+1])
    if (dim(data_temp)[1]==0) {
        print(paste("No row found with this conditions, i = ",i))
    } else {
        print(paste("data_temp:",dim(data_temp)[1]))
        p1 <- ggplot(data_temp, aes(Tw, speed_m_s))+
        geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
        theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle=90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25))+
        labs(x = "Temperature [°C]",
            y = "speed_m_s")

        p2 <- ggplot(data_temp, aes(Q, speed_m_s))+
        geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
        theme(
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle=90),
        axis.title.x = element_text(size = 25),
        axis.text.y = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25))+
        labs(x = "Discharge [m^3/s]",
            y = "speed_m_s")
        #print(p2 / p1 + plot_layout(heights = c(1,2)))
        print(p1)
        }
}
dev.off()