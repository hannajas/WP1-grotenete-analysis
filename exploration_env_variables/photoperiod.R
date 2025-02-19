library(tidyverse)
library(lubridate)
library(tidyquant)

env_data <- read_csv('data/raw/photoperiod_verwerkt.csv')
env_data$photoperiod <- factor(as.numeric(hms(env_data$photoperiod),"minutes"))
env_data$date <- factor(env_data$date)
env_data$id <- 1:nrow(env_data)

p <- ggplot(data = env_data,
mapping = aes (x = date,
y = photoperiod)) +
geom_point () +
labs(title = "Photoperiod vs ID",
    x = "date",
    y = "Photoperiod (minutes)")
print(p)
#h <- ggplot()
#h <- h + geom_point(aes(id, photoperiod), data = env_data, shape = 16, size = 5)
#View(h)