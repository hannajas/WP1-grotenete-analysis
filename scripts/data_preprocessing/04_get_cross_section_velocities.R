# Calculation of the water velocity at 5 location with Q and H-area data (obtained by Maarten Deschamps HIC)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(patchwork)

source('./src/get_cross_section_velocity_function.R')
###############################################################################################################################
# load data
###############################################################################################################################
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#gnt07a_1066 -> Q and H (resolution 15 min)
#gnt05a_1066 -> Q and H (resolution 5 min)

#Rupel
#BS-RUP-1095 (=rup02a-1066) -> H (resolution 5 min)
#rup00a_1066 -> Q (1 day)

#Scheldt 1
#zes28a_1066 -> H (1 min)
#zes29f_1066 -> Q (1 day)

#Scheldt 2
#zes01a_1066 -> H (1 min)
#zes00a_1066 -> Q (1 day)

###############################################################################################################################
# process data
###############################################################################################################################
Q_station_names <- c(
  "gnt07a_1066",
  "gnt05a_1066",
  "rup00a_1066",
  "zes29f_1066",
  "zes00a_1066"
)
H_station_names <- c(
  "gnt07a_1066",
  "gnt05a_1066",
  "BS_RUP_1095",
  "zes28a_1066",
  "zes01a_1066"
)

for (i in 1:length(Q_station_names)) {
  temp <- get_velocity(Q_station_names[i], H_station_names[i], metadata)
  write.csv(
    temp,
    paste(
      "C:/Code/WP1-grotenete-analysis/data/interim/processed/",
      H_station_names[i],
      "_V.csv",
      sep = ""
    ),
    row.names = FALSE
  )
}
