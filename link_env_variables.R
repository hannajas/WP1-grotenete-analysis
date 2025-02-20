library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/inverse_distance_function.R")
source("./src/concat_all_env_var_functions.R")

# Upload dataset
data <- read_csv('./data/interim/migration.csv', show_col_types = FALSE)
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# META-DATA
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
n_data <- dim(metadata)[1] #number of variables
metadata$resolution <- NA
for (i in 1:n_data) {
    metadata$resolution[i] <- toString(as.period(metadata$resolution_multiplier[i],metadata$resolution_unit[i]))
}
receiver <- lapply(1:n_data, function(i) {
    data$station_name[which(round(data$distance_to_source_m, digits = 2) == round(metadata$distance_to_source[i], digits=2))][1]
    })
metadata$receiver <- unlist(receiver)


# averaging the environmental variables to fit telemetry data
env_data_Tw <- concat_all_env_vars(data, "Tw", metadata)
env_data_Q <- concat_all_env_vars(data, "Q", metadata)
env_data_photoperiod <- concat_all_env_vars(data, "photoperiod", metadata)


# INVERSE DISTANCE WEIGHTING
p <- 1
data$Tw <- inverse_distance(data, env_data_Tw, metadata,p,"Tw") # in deze functie nog filteren in meta data
data$Q <- inverse_distance(data, env_data_Q, metadata,p,"Q")
data$photoperiod <- env_data_photoperiod$photoperiod

#CALCULATE THE DELTA VALUES
data_list <- split(data, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
    x$delta_Tw <- c(NaN,diff(x$Tw))
    x$delta_Q <- c(NaN,diff(x$Q))
    return(x)
})
data <- plyr::ldply(data_temp, data.frame)

# filter out unrealistic speed values
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten



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
labs(x = "Debiet [m^3/s]",
    y = "speed_m_s")
#ggsave('./figures/correlationswatertemperature.png')

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
d1 <- d1 + xlim(-6,3)


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

p <- ggplot(data_filter, aes(Q, speed_m_s))+
geom_point(shape = 16, size = 5)+ geom_smooth(method=lm, size = 2)+
theme(
axis.line = element_line(colour = "black"),
axis.text.x = element_text(size = 20, colour = "black", angle=90),
axis.title.x = element_text(size = 25),
axis.text.y = element_text(size = 25, colour = "black"),
axis.title.y = element_text(size = 25))
#ggsave('./figures/correlations/discharge.png')


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