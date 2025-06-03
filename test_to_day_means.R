library(adehabitatLT)
library(tidyverse)
library(rvest)
###################################################################################
#Progress:
# gelukt om afgelegde afstand per dag te berekenen
# HOE HIERAAN V_W TE KOPPELEN IS ME NOG NIET DUIDELIJK!
# je zou telkens de locatie waar hij zich te midden van de dag bevind moeten gebruiken als interpolatie locatie (inverse distance)
# vinden LOCATIE midden dag
     #LANGS TALLIJN (geen idee ho dit te doen)
#DICHTSBIJZIJNDE RECEIVER vinden bij deze middeldag locatie om inverse distance te doen (inverse_distance_function.R)


###################################################################################


#average eel speed
data_filter <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
data_filter$middledate <- as.POSIXct(round_date(data_filter$arrival+data_filter$residence/2, "15 mins"))


data_filter <- data_filter %>%#BUG IN THE ADEHABITAT R PACKAGE! these columns should be named x and y
  rename(
    x = deploy_longitude,
    y = deploy_latitude
    )
xy <- data_filter[,c("x", "y")]
date <- data_filter$middledate
id <- as.character(data_filter$tag_serial_number)
traj <- as.ltraj(xy,data_filter$middledate, id)

result_df <- data.frame(date = as.Date(character()), speed_m_s = numeric(),id = character())

for (j in 1:length(traj)) {
    traj_1 <- redisltraj(traj[j], u = 60*15, type = "time")[[1]]
    if (nrow(traj_1) == 1) {
        next
    }
    data_day <- data.frame(
    date = seq(ceiling_date(traj_1$date[1], "day"), floor_date(traj_1$date[nrow(traj_1)], "day"), by="days"),
    speed_m_s = NA,
    id = unique(id)[j]
    )
    for (i in 1:nrow(data_day)) {
        traj_1_day <- traj_1 %>%
            filter(
                floor_date(date, "day") == data_day$date[i]
            )
        data_day$speed_m_s[i] <- sum(traj_1_day$dist)/(24*60*60) # in m/s
    }
    result_df <- rbind(result_df, data_day)
}


# average water velocity
metadata_V <- filter(metadata, metadata$type == "V")
#om dit properly te doen moeten we voor elk punt in het geinterpoleerde traject de distance to source kennen!






























#########################################################################################################
# test
data_filter <- data_filter %>%
    mutate(midnight = floor_date(middledate, "day") %within% (middledate %--% lag(middledate)))
data_filter$weight <- NA
for (i in 2:nrow(data_filter)) {
    if (data_filter$midnight[i] == TRUE) {
        data_filter$weight[i] <- int_length(data_filter$middledate[i] %--% floor_date(data_filter$middledate[i], "day"))# nog mee rekeing houden als de paling er meer dan 1 DAG OVER DOET
    } else {
        data_filter$weight[i] <- int_length(data_filter$middledate[i] %--% data_filter$middledate[i-1])
    }
}

data_eels <- data_filter %>%
    filter(tag_serial_number == 1171746)

data_day <- data.frame(
    date = seq(floor_date(data_eels$arrival[1], "day"), floor_date(data_eels$arrival[nrow(data_eels)], "day"), by="days"),
    speed_m_s = NA
)

for (i in 1:nrow(data_day)) {
    data_day_filter <- data_eels %>%
        filter(floor_date(arrival,"day") == data_day$date[i] | floor_date(departure,"day") == data_day$date[i])
    index <- which(floor_date(arrival,"day") == data_day$date[i] | floor_date(departure,"day") == data_day$date[i])
    if (nrow(data_day_filter) > 1) {
            if (abs(sum(data_day_filter$weight)) < 86400) {
                weight_pl <- 86400 - abs(sum(data_day_filter$weight))
                velocity_pl <- data_eels$speed_m_s[index[length(index)]]
                data_day$speed_m_s[i] <- weighted.mean(c(data_day_filter$speed_m_s, velocity_pl), c(data_day_filter$weight, weight_pl))
            } else {
                data_day$speed_m_s[i] <- weighted.mean(data_day_filter$speed_m_s, data_day_filter$weight)
            }

    }
    if (nrow(data_day_filter) == 0) {
        data_day$speed_m_s[i] <- data_day_filter$speed_m_s
    }
    if (nrow(data_day_filter) == 1) {
        data_day$speed_m_s[i] <- data_day_filter$speed_m_s
    }
}
