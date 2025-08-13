# Preprocessing watertemperature data + look at correlation between temperature measurements
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(ggplot2)
library(patchwork)
library(lubridate)

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
begin1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-03 09:00:00 UTC"))
eind1 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-08 09:30:00 UTC"))
begin2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-19 09:30:00 UTC"))
eind2 <- which(L07_077_Tw$Timestamp == ymd_hms("2019-05-28 12:30:00 UTC"))
L07_077_Tw$Value[begin1:eind1] <- NA
L07_077_Tw$Value[begin2:eind2] <- NA
L07_077_Tw$Value <- as.numeric(L07_077_Tw$Value)
#L07_077_Tw$Timestamp <- ymd_hms(L07_077_Tw$Timestamp)
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
L07_077_Tw <- read_csv('./data/interim/processed/L07_077_Tw.csv')
rup02e_SF_1066_Tw <- read_csv('./data/interim/processed/rup02e_SF_1066_Tw.csv')
Tw <- left_join(L07_077_Tw, rup02e_SF_1066_Tw, by = "Timestamp")
Tw <- Tw %>%
  rename(
    Grote_nete_geel = Value.x,
    Rupel = Value.y
  )

# plot one temperature
g <- ggplot() +
  geom_line(
    aes(Timestamp, Value),
    data = L07_077_Tw[
      (L07_077_Tw$Timestamp >= "2019-12-10" &
        L07_077_Tw$Timestamp <= "2019-12-16"),
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
g <- ggplot() +
  geom_line(aes(Timestamp, Grote_nete_geel), data = Tw, colour = "green") +
  geom_line(aes(Timestamp, Rupel), data = Tw, colour = "blue") +
  theme(legend.position = "top")

#correlation plot
g1 <- ggplot() +
  geom_line(aes(Rupel, Grote_nete_geel), data = Tw, colour = "blue")

#correlation
cor(Tw$Grote_nete_geel, Tw$Rupel, use = "complete.obs")

#correlation test -> no need! we are not looking of the 'population parameter' for this 'sample' of date

## testing the assumptions
g2 <- qqline(Tw$Grote_nete_geel)
# --> temperature is not independent (time dependent) AND not normally distributed!

cor.test(Tw$Grote_nete_geel, Tw$Rupel, use = "complete.obs") #houdt geen steek want er wordt niet aan de assumpties voldaan
