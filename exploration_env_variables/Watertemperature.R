# Preprocessing watertemperature data + look at correlation between temperature measurements
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(ggplot2)
library(patchwork)
library(lubridate)
library(dplyr)
library(tidyr)

# read raw data
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_Tw <- filter(metadata, metadata$type == "Tw")
n <- dim(metadata_Tw)[1]

for (i in 1:n) {
  path <- paste(
    './data/raw/temperature/',
    metadata_Tw$name[i],
    '_Tw.csv',
    sep = ""
  )
  temp <- read_csv(path)
  assign(paste(metadata_Tw$name[i], '_Tw', sep = ""), temp)
}

# PRE PROCESSING!!!
# unreliable values --> NA (I did a manual screen)
begin1 <- which(L10_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L10_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L10_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L10_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
# insert timestamps 25-10-2019 13u00 - 29-10-2019 07u15
time_step <- seq(
  ymd_hms("2019-10-25 13:00:00 UTC"),
  ymd_hms("2019-10-29 07:15:00 UTC"),
  by = "15 min"
)
missing_times <- data.frame(Timestamp = time_step)
# Join with the original data to ensure all time_step rows are present
L10_077_Tw <- full_join(L10_077_Tw, missing_times, by = "Timestamp") %>%
  arrange(Timestamp)


L10_077_Tw$Value[begin1:eind1] <- NA
L10_077_Tw$Value[begin2:eind2] <- NA
L10_077_Tw$Value <- as.numeric(L10_077_Tw$Value)
#L10_077_Tw$Timestamp <- ymd_hms(L10_077_Tw$Timestamp)
#rup02e_SF_1066_Tw$Timestamp <- ymd_hms(rup02e_SF_1066_Tw$Timestamp)

# remove "...2" column out of zes24a_SF_1066_Tw.csv
zes24a_SF_1066_Tw <- zes24a_SF_1066_Tw[, -c(2)]

for (i in 1:n) {
  path <- paste(
    './data/interim/processed/',
    metadata_Tw$name[i],
    '_Tw.csv',
    sep = ""
  )
  write.csv(get(paste(metadata_Tw$name[i], '_Tw', sep = "")), path)
}


# load the processed data
zes28a_SF_1066_Tw <- read_csv('./data/interim/processed/zes28a_SF_1066_Tw.csv')
L10_077_Tw <- read_csv('./data/interim/processed/L10_077_Tw.csv')
rup02e_SF_1066_Tw <- read_csv('./data/interim/processed/rup02e_SF_1066_Tw.csv')
Tw <- left_join(L10_077_Tw, rup02e_SF_1066_Tw, by = "Timestamp")
Tw <- Tw %>%
  rename(
    Grote_nete_geel = Value.x,
    Rupel = Value.y
  )

# plot one temperature
g <- ggplot() +
  geom_line(
    aes(Timestamp, Value),
    data = L10_077_Tw[
      (L10_077_Tw$Timestamp >= "2019-12-10" &
        L10_077_Tw$Timestamp <= "2019-12-16"),
    ],
    colour = "green"
  ) +
  theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 day") +
  theme(
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )


# overview of values
Tw_long <- Tw %>%
  dplyr::select(Timestamp, Grote_nete_geel, Rupel) %>%
  pivot_longer(
    cols = c(Grote_nete_geel, Rupel),
    names_to = "Location",
    values_to = "Temperature"
  )

g <- ggplot(Tw_long, aes(x = Timestamp, y = Temperature, colour = Location)) +
  geom_line() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 18), # bigger legend text
    legend.title = element_text(size = 20),
    axis.title = element_text(size = 18)
  ) + #bigger legend
  labs(colour = "Location")
ggsave(g, file = "./figures/Temperature/Rupel_Vs_Nete.png")

#correlation plot
g1 <- ggplot() +
  geom_line(aes(Rupel, Grote_nete_geel), data = Tw, colour = "blue")

#correlation
cor(Tw$Grote_nete_geel, Tw$Rupel, use = "complete.obs")

#correlation test -> no need! we are not looking of the 'population parameter' for this 'sample' of date

## testing the assumptions
qqnorm(Tw$Grote_nete_geel)
qqline(Tw$Grote_nete_geel)
ggplot(Tw, aes(x = Rupel)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of Tw", x = "Tw", y = "Count")
# --> temperature is not independent (time dependent) AND not normally distributed!

cor.test(Tw$Grote_nete_geel, Tw$Rupel, use = "complete.obs") #houdt geen steek want er wordt niet aan de assumpties voldaan


###################################################################################################################
# CALCULATE ANOMALIE OF THE CLIMATOLOGY
# https://www.r-bloggers.com/2020/03/visualize-climate-anomalies/
# 1. climatology
ts_id <- c(39305042, 103542010, 45540010, 110824010, 51824010)
name <- c(
  "L10_077",
  "rup02e_SF_1066",
  "zes24a_SF_1066",
  "zes09x_SF_1066",
  "zes01a_SF_1066"
)
source <- c(1, 4, 4, 4, 4)
for (i in 1:length(ts_id)) {
  Tw <- get_timeseries_tsid(
    ts_id[i],
    from = "2000-01-01",
    to = "2025-01-01",
    datasource = 4
  )
  path <- paste('./data/anomaly/', name[i], '_Tw.csv', sep = "")
  write.csv(Tw, path)
}
