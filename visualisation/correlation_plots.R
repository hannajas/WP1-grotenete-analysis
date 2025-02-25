data_filter <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)

# CORRELATION PLOT
# Tw
p <- ggplot(data_filter, aes(Tw, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Tw [m^3/s]",
    y = "speed_m_s")
#ggsave('./figures/correlations/watertemperature.png')

d1 <- ggplot()
d1 <- d1 + geom_point(aes(Tw, speed_m_s), data = data_filter, shape = 16, size = 5)
d1 <- d1 + geom_smooth(method=lm)
d1 <- d1 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
#d1 <- d1 + xlim(-6,3)
#ggsave('./figures/correlations/delta_watertemperature.png')

g <- ggplot(data_filter, aes(Tw, downstream_migration))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))
#ggsave('./figures/watertemperature_mirgation.png')

d1 <- ggplot()
d1 <- d1 + geom_point(aes(delta_Tw, speed_m_s), data = data_filter, shape = 16, size = 5)
d1 <- d1 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
#ggsave('./figures/correlations/delta_watertemperature.png')

# Q
p1 <- ggplot(data_filter, aes(Q, downstream_migration))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Debiet [m^3/s]",
    y = "migration")

p1 <- ggplot(data_filter, aes(Q, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))
p1 <- p1 + scale_x_log10(guide = "axis_logticks")
#ggsave('./figures/correlations/log_discharge.png')

Q_delta <- ggplot(data_filter, aes(delta_Q, speed_m_s))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))


#photoperiod
d3 <- ggplot()
d3 <- d3 + geom_point(aes(photoperiod, speed_m_s), data = data_filter, shape = 16, size = 5)
d3 <- d3 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d3 <- d3 +   labs(x = "photoperiod [min]",y = "speed_m_s")
d3 <- d3 + xlim(460,700)

d2 <- ggplot()
d2 <- d2 + geom_point(aes(downstream_migration, photoperiod), data = data_filter, shape = 16, size = 5)
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d2 <- d2 +   labs(x = "photoperiod [min]",y = "migration")
# ggsave('./figures/photoperiod_migration.png')

d2 <- ggplot()
d2 <- d2 + geom_point(aes(delta_photoperiod, speed_m_s), data = data_filter, shape = 16, size = 5)
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))
d2 <- d2 + xlim(-5,5)

d2 <- d2 +   labs(x = "photoperiod [min]",y = "migration")





# RAINFALL
R1 <- ggplot(data_filter, aes(R, speed_m_s))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "R [mm]",
    y = "speed_m_s")


# SALINITY
grenswaardes_distance_to_source <- c(43106.96,70306.3)
data_filter <- filter(data, !startsWith(data$station_name, "ws-") & (data$distance_to_source_m> 70306.3)) #+- 62 waarden uitgelaten

S_zes <- ggplot(data_filter, aes(S, speed_m_s))+
geom_point(shape = 16, size = 5)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "S [mm]",
    y = "speed_m_s")
#ggsave('./figures/correlations/salinity.png')




# TURBIDITY
data_filter <- filter(data, !startsWith(data$station_name, "ws-"))

T1 <- ggplot(data_filter, aes(turb, speed_m_s))+
geom_point(shape = 16, size = 5)+#geom_smooth(method = lm,formula = y ~ log(x))+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "turb [NTU]",
    y = "speed_m_s")
#ggsave('./figures/correlations/salinity.png')

# OXYGEN
data_filter <- filter(data, !startsWith(data$station_name, "ws-"))

O1 <- ggplot(data_filter, aes(O, speed_m_s))+
geom_point(shape = 16, size = 5)+#geom_smooth(method = lm,formula = y ~ log(x))+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "O [mg/l]",
    y = "speed_m_s")
O1 <- O1 + xlim(4,11)


#######################################################################################################################

# PLOTTING EACH SEGMENT
# Bv. voor traject gn-13 tot gn-11
data_sort <- data_filter[order(data_filter$distance_to_source_m),]
#datasort <- sort(data_filter$distance_to_source_m)
stations <- unique(data_sort$station_name)
# skip stations in Scheldt more upstream than more upstream than Rupel
seq_af <- stations[stations != "s-7" & stations != "s-6" & stations != "s-5" & stations != "s-4c" & stations != "s-4b" & stations != "s-4a"]
#skip release location 3 en 2
seq_af <- seq_af[seq_af != "rel_grotenete2" & seq_af != "rel_grotenete3"]


# plot the speed of the fish for each segment
pdf("./figures/Downstream_segments_Tw_Q.pdf") # Create pdf
for (i in 2:length(seq_af)-1){

    data_temp <- filter(data_filter, dplyr::lag(station_name) == seq_af[i] & station_name == seq_af[i+1])
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
    geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, linewidth = 2)+
    theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25))+
    labs(x = "Discharge [m^3/s]",
        y = "speed_m_s")
    p2 <- p2 + labs(title = paste("From segment",seq_af[i],"to",seq_af[i+1]))
    print(p2 / p1 + plot_layout(heights = c(1,1)))
    #print(p1)
}
dev.off()#PROBLEEM MET HET OPENEN

# welk soort traject wordt he meeste getraceerd?

gn13_11 <- filter(data_filter, dplyr::lag(station_name) == "gn-13" & station_name == "gn-11")
p <- ggplot(gn13_11, aes(Tw, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Debiet [m^3/s]",
    y = "speed_m_s")

gn9_7 <- filter(data_filter, dplyr::lag(station_name) == "gn-9" & station_name == "gn-7")
p <- ggplot(gn9_7, aes(Q, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))+
labs(x = "Debiet [m^3/s]",
    y = "speed_m_s")