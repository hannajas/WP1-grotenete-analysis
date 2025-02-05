library(wateRinfo)
library(tidyverse)
library(rvest)

# WATER TEMPERATURE

#to get the ts_id
# print(get_stations("water_temperature"))
# L10_077 --> ts_id = 39305042
L07_077_Tw <- get_timeseries_tsid("39305042", from = "2019-01-01", to = "2021-02-28") #resolution = 15 minutes

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

base <- "hic"
rup02e_SF_1066_Tw <- get_timeseries_tsid("103542010", from = "2019-01-01", to = "2021-02-28", datasource=4)
write.csv(rup02e_SF_1066_Tw, './data/raw/rup02e_SF_1066_Tw.csv')

base <- "hic"
zes28a_SF_1066_Tw <- get_timeseries_tsid("45540010", from = "2019-01-01", to = "2021-02-28", datasource=4)
write.csv(zes28a_SF_1066_Tw, './data/raw/zes28a_SF_1066_Tw.csv')

zes09x_SF_1066  <- get_timeseries_tsid("110824010", from = "2019-01-01", to = "2021-02-28", datasource=4)
write.csv(zes09x_SF_1066, './data/raw/zes09x_SF_1066_Tw.csv')

zes01a_SF_1066  <- get_timeseries_tsid("51824010", from = "2019-01-01", to = "2021-02-28", datasource=4)
write.csv(zes01a_SF_1066, './data/raw/zes01a_SF_1066_Tw.csv')

# DEBIET ################################################################################################

# find timeseries ID by https://download.waterinfo.be/tsmdownload/KiWIS/KiWIS?datasource=1&service=kisters&type=queryServices&request=getTimeseriesList&datasource=0&format=html&station_no=L10_077&parametertype_name=Q

# take the Pv.15 series
# ts_id: 69694042
L10_077_Q <- get_timeseries_tsid("69694042", from = "2019-01-01 UTC", to = "2021-02-28 UTC", datasource=1)
write.csv(L10_077_Q, './data/raw/discharge/L10_077_Q.csv')

gnt07a_1066_Q <- get_timeseries_tsid("67302010", from = "2019-01-01 UTC", to = "2021-02-28 UTC", datasource=4)
write.csv(gnt07a_1066_Q, './data/raw/discharge/gnt07a_1066_Q.csv')

gnt05a_1066_Q <- get_timeseries_tsid("69196010", from = "2019-01-01 UTC", to = "2021-02-28 UTC", datasource=4)
write.csv(gnt05a_1066_Q, './data/raw/discharge/gnt05a_1066_Q.csv')

rup00a_1066_Q <- get_timeseries_tsid("75944010", from = "2019-01-01 UTC", to = "2021-02-28 UTC", datasource=4)
write.csv(rup00a_1066_Q, './data/raw/discharge/rup00a_1066_Q.csv')