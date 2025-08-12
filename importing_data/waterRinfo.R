# Importing environmental data by making use of the wateRinfo package
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(wateRinfo)
library(tidyverse)
library(rvest)

# WATER TEMPERATURE

#to get the ts_id
# print(get_stations("water_temperature"))
# L10_077 --> ts_id = 39305042
L07_077_Tw <- get_timeseries_tsid(
  "39305042",
  from = "2019-01-01",
  to = "2021-02-28"
) #resolution = 15 minutes

g <- ggplot(L07_077_Tw, aes(Timestamp, Value)) +
  geom_line()
write.csv(L07_077_Tw, './data/raw/L07_077_Tw.csv')

#rup02e-SF-1066
#to get the timeseriesgroup_id: https://hicws.vlaanderen.be/Manual_for_the_use_of_webservices_HIC.pdf
#to get the metadata of all the timeseries in this group:
# https://hicws.vlaanderen.be/KiWIS/KiWIS?datasource=4&type=queryServices&request=gettimeseriesvalues&service=kisters&timeseriesgroup_id=156200&metadata=true
# OF
# https://waterinfo.vlaanderen.be/tsmhic/KiWIS/KiWIS?&type=queryServices&service=kisters&datasource=4&request=getTimeseriesValues&ts_id=103542010&format=html&from=2019-01-01T00:00:00+01:00&to=2021-02-28T23:00:+01:00
#ts_id = 103542010
ts_id <- c(103542010, 45540010, 110824010, 51824010)
name <- c(
  "rup02e_SF_1066",
  "zes24a_SF_1066",
  "zes09x_SF_1066",
  "zes01a_SF_1066"
)
for (i in 1:length(ts_id)) {
  Tw <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01",
    to = "2021-02-28",
    datasource = 4
  )
  path <- paste('./data/raw/', name[i], '_Tw.csv', sep = "")
  write.csv(Tw, path)
}

# DEBIET ################################################################################################

# find timeseries ID by https://download.waterinfo.be/tsmdownload/KiWIS/KiWIS?datasource=1&service=kisters&type=queryServices&request=getTimeseriesList&datasource=0&format=html&station_no=L10_077&parametertype_name=Q

# take the Pv.15 series
ts_id <- c(69694042, 67302010, 69196010, 75944010, 83735010, 67748010)
name <- c(
  "L10_077",
  "gnt07a_1066",
  "gnt05a_1066",
  "rup00a_1066",
  "zes29f_1066",
  "zes00a_1066"
)
for (i in 1:length(ts_id)) {
  Q <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 1
  )
  path <- paste('./data/raw/discharge/', name[i], '_Q.csv', sep = "")
  write.csv(Q, path)
}

######################################################################
# RAINFALL
stations <- get_stations("rainfall", frequency = "15min") %>%
  filter(station_longitude > 4 & station_latitude > 50.88)
for (i in 1:nrow(stations)) {
  ts_id <- stations$ts_id[i]
  station_name <- stations$station_no[i]
  print(station_name)
  print(ts_id)
  rainfall <- get_timeseries_tsid(
    ts_id,
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC"
  )
  path <- paste('./data/raw/rainfall/', station_name, '_R.csv', sep = "")
  write.csv(rainfall, path)
}


######################################################################
# salinity
ts_id <- c(110938010, 46619010, 103659010)
name <- c("zes09x-SF-1066", "zes24a-SF-1066", "rup02e-SF-1066")
for (i in 1:length(ts_id)) {
  salinity <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 4
  )
  path <- paste('./data/raw/salinity/', name[i], '_S.csv', sep = "")
  write.csv(salinity, path)
}


######################################################################
# turbidity
ts_id <- c(110971010, 46651010, 103692010)
name <- c("zes09x_SF_1066", "zes24a_SF_1066", "rup02e_SF_1066")
for (i in 1:length(ts_id)) {
  turbidity <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 4
  )
  path <- paste('./data/raw/turbidity/', name[i], '_turb.csv', sep = "")
  write.csv(turbidity, path)
}


######################################################################
# oxygen
ts_id <- c(110898010, 46561010, 103603010)
name <- c("zes09x_SF_1066", "zes24a_SF_1066", "rup02e_SF_1066")
for (i in 1:length(ts_id)) {
  oxygen <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 4
  )
  path <- paste('./data/raw/oxygen/', name[i], '_O.csv', sep = "")
  write.csv(oxygen, path)
}


######################################################################
#tides
name <- c(
  "bnt07a_1066",
  "bnt03a_1066",
  "bnt01c_1066",
  "zes28a_1066",
  "zes21a_1066",
  "zes14a_1066",
  "zes10a_1066",
  "zes01a_1066"
)
ts_id <- c(
  "54829010",
  "54289010",
  "114027010",
  "54499010",
  "53995010",
  "54612010",
  "54942010",
  "54581010"
) #ts_id_getij <- read.csv("./data/tij_all_identifiers.txt")
for (i in 1:length(ts_id)) {
  tide <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 4
  )
  path <- paste('./data/raw/tide/', name[i], '_tij.csv', sep = "")
  write.csv(tide, path)
}


##############################################################################
name <- c(
  "gnt07a_1066",
  "gnt05a_1066",
  "BS_RUP_1095",
  "zes28a_1066",
  "zes01a_1066"
)
ts_id <- c("67324010", "69227010", "	54090010", "54493010", "56088010")
for (i in 1:length(ts_id)) {
  tide <- get_timeseries_tsid(
    ts_id[i],
    from = "2019-01-01 UTC",
    to = "2021-02-28 UTC",
    datasource = 4
  )
  path <- paste('./data/raw/level/', name[i], '_H.csv', sep = "")
  write.csv(tide, path)
}

BS_RUP_1095 <- get_timeseries_tsid(
  "54090010",
  from = "2019-01-01 UTC",
  to = "2021-02-28 UTC",
  datasource = 4
)
write.csv(BS_RUP_1095, './data/raw/level/BS_RUP_1095_H.csv')

metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)
metadata_H <- filter(metadata, metadata$type == "H")

for (i in 1:nrow(metadata_H)) {
  path <- paste('./data/raw/level/', metadata_H$name[i], '_H.csv', sep = "")
  temp <- read_csv(path)
  assign(paste(metadata_H$name[i], '_H', sep = ""), temp)
}

for (i in 1:nrow(metadata_H)) {
  path <- paste(
    './data/interim/processed/',
    metadata_H$name[i],
    '_H.csv',
    sep = ""
  )
  write.csv(get(paste(metadata_H$name[i], '_H', sep = "")), path)
}
