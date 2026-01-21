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
data_sort <- data[order(data$speed_m_s, decreasing = FALSE), ]
data_slow <- filter(data, data$speed_m_s< 0.12)
data_fast <- filter(data, data$speed_m_s> 0.08)

g <- ggplot()
g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
g <- g + geom_density(aes(x = speed_m_s), data = data_slow, fill="#69b3a2", color="#e9ecef", alpha=0.8)
g <- g + scale_x_log10(guide = "axis_logticks")
g <- g + geom_vline(xintercept=0.01, linetype = "dotted", linewidth = 1)
print(g)
#ggsave('./figures/speed_m_s_distribution.png')

################################################################################################
# Figure in pwp Phd Day - speed distribution

# Define the x values
x <- seq(-10, 20, length.out = 1000)

# Define two normal distributions
# Load ggplot2 package
library(ggplot2)

# Define the x values
x <- seq(0, 4, length.out = 1000)

# Define two normal distributions
peak1 <- 0.3 * dnorm(x, mean = 1, sd = 0.5)
peak2 <- 1.8 * dnorm(x, mean = 2.5, sd = 0.5)

# Combine the data into a data frame
data <- data.frame(
  x = x,
  combined = peak1 + peak2
)

# Plot using ggplot2
ggplot(data, aes(x = x, y = combined)) +
  geom_line(color = "blue", size = 2) +
  labs(
    title = "Normal Distribution with Two Peaks",
    x = "X-axis",
    y = "Probability Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
ggsave('./figures/pwp_normal_dist.png')

data <- data.frame(
  x = x,
  combined = peak2
)

ggplot(data, aes(x = x, y = combined)) +
  geom_line(color = "blue", size = 2) +
  labs(
    title = "Normal Distribution with Two Peaks",
    x = "X-axis",
    y = "Probability Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )+
  ylim(0, 1.5)

ggsave('./figures/pwp_peak_2.png')


######################################################################################
# pwp - labelling by trajectory analysis 
#labelled

# eel 1294161
data <- read_csv('./data/raw/migration.csv') 
data$...1 <- NULL
data$tag_serial_number <- factor(data$tag_serial_number)
data$migration <- factor(data$migration)
data$station_name <- factor(data$station_name)
## Create single plot

# Select individual
data2 <- data[which(data$tag_serial_number == "1294161"), ]
#data2=data2[order(as.POSIXct(strptime(data2$Arrival,"%d/%m/%Y %H:%M"))),]
data2 <- data2[order(as.POSIXct(strptime(data2$arrival,"%Y-%m-%d %H:%M:%S"))),]

#manipulate the data
data2$downstream_migration[c(21,24)] <- FALSE
data2$downstream_migration[c(35,36)] <- TRUE

# Create plot
ggplot() + geom_line(aes(arrival, -1*distance_to_source_m/1000), data = data2, colour = "black", size = 2) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000, colour = downstream_migration), data = data2, shape = 16, size = 7) +
  scale_color_manual(values = c("FALSE" = "red",
                                "TRUE" =  "green")) +
  #ggtitle(data2$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#  scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25)) +
  scale_x_datetime(date_breaks  ="1 week") + 
  geom_hline(yintercept = -1*data2$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")
#ggsave('./figures/distance_plot_labelled.png')


#grey
ggplot() + geom_line(aes(arrival, -1*distance_to_source_m/1000), data = data2, colour = "black", size = 2) + 
  geom_point(aes(arrival, -1*distance_to_source_m/1000), data = data2, shape = 16, size = 7,colour = "grey") +
  #ggtitle(data2$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) +
  ylab("Distance (km)") +
  xlab("Date") + 
#  scale_y_continuous(limit = c(0, 60000),breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000), labels = c(0,10,20,30,40,50,60)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle=90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25)) +
  scale_x_datetime(date_breaks  ="1 week") + 
  geom_hline(yintercept = -1*data2$distance_to_source_m/1000, colour = "gray", size = 0.5, linetype = "dashed") +
  #annotate("text",x = data2$arrival[1] - (300*60*60), y = data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 5) +
  theme(legend.position="none")
ggsave('./figures/distance_plot_grey.png')
