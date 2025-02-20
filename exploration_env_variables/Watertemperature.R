library(ggplot2)
library(patchwork)
library(lubridate)

# read raw data
for (i in 1:n) {
    path <- paste('./data/raw/temperature/',metadata_Tw$name[i],'_Tw.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_Tw$name[i],'_Tw', sep =""), temp)
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

for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_Tw$name[i],'_Tw.csv', sep ="")
    write.csv(get(paste(metadata_Tw$name[i],'_Tw', sep ="")), path)
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
g <- ggplot()
g <- g + geom_line(aes(Timestamp, Value), data = rup02e_SF_1066_Tw, colour = "green")
g <- g + theme(legend.position="top")

# overview of values
g <- ggplot()
g <- g + geom_line(aes(Timestamp, Grote_nete_geel), data = Tw, colour = "green")
g <- g + geom_line(aes(Timestamp, Rupel), data = Tw, colour="blue")
g <- g + theme(legend.position="top")

#correlation plot
g1 <- ggplot()
g1 <- g1 + geom_line(aes(Rupel, Grote_nete_geel), data = Tw, colour="blue")

#correlation
cor(Tw$Grote_nete_geel, Tw$Rupel, use="complete.obs")

#correlation test -> no need! we are not looking of the 'population parameter' for this 'sample' of date

## testing the assumptions
g2 <- ggplot()
g2 <- g2 + qqnorm(Tw$Grote_nete_geel)
g2 <- g2 + qqline(Tw$Grote_nete_geel)
# --> temperature is not independent (time dependent) AND not normally distributed!

cor.test(Tw$Grote_nete_geel, Tw$Rupel, use="complete.obs")#houdt geen steek want er wordt niet aan de assumpties voldaan
