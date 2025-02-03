library(ggplot2)
library(patchwork)
library(lubridate)
zes28a_SF_1066_Tw <- read_csv('./data/raw/zes28a_SF_1066_Tw.csv')
L07_077_Tw <- read_csv('./data/raw/L07_077_Tw.csv')
# zero values cannot be trusted AND some values surrounding this! (see Datatype.docx)
L07_077_Tw$Value[L07_077_Tw$Value == 0] <- "NA"
rup02e_SF_1066_Tw <- read_csv('./data/raw/rup02e_SF_1066_Tw.csv')
Tw <- left_join(L07_077_Tw, rup02e_SF_1066_Tw, by = "Timestamp")
Tw %>%
  rename(
    Grote_nete_geel = Value.x,
    Rupel = Value.y
    )

# plot one temperature
g <- ggplot()
g <- g + geom_line(aes(Timestamp, Value), data = zes28a_SF_1066_Tw, colour = "green")
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
