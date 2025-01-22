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

g <- ggplot()
g <- g + theme(axis.text.x = element_text(size = 14, colour = "black", angle=90),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16), axis.text.y = element_text(size = 14))
g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
g <- g + geom_density(aes(x = speed_m_s), data = data, fill="#69b3a2", color="#e9ecef", alpha=0.8)
g <- g + scale_x_log10(guide = "axis_logticks")
g <- g + geom_vline(xintercept=0.01, linetype = "dotted", linewidth = 1)
print(g)
ggsave('./figures/speed_m_s_distribution.png')