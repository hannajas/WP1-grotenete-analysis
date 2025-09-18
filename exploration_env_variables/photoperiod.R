# load raw photoperiod
# preprocessing: Timestamp juist formatten,...
# write to /interim folder
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(tidyverse)
library(lubridate)
library(tidyquant)

metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_p <- filter(metadata, metadata$type == "photoperiod")

photoperiod_photoperiod <- read_csv('data/raw/photoperiod_verwerkt.csv') #komt NIET vanop waterinfo (https://robinfo.oma.be/en/)
photoperiod_photoperiod$Value <- factor(as.numeric(
  hms(photoperiod_photoperiod$photoperiod),
  "minutes"
))
photoperiod_photoperiod$Timestamp <- factor(photoperiod_photoperiod$date)
photoperiod_photoperiod$id <- 1:nrow(photoperiod_photoperiod)

path <- paste(
  './data/interim/processed/',
  metadata_p$name[1],
  '_photoperiod.csv',
  sep = ""
)
write.csv(get(paste(metadata_p$name[1], '_photoperiod', sep = "")), path)
photoperiod_photoperiod <- read_csv(
  "./data/interim/processed/photoperiod_photoperiod.csv"
)

p <- ggplot(data = env_data, mapping = aes(x = date, y = photoperiod)) +
  geom_point() +
  labs(title = "Photoperiod vs ID", x = "date", y = "Photoperiod (minutes)")
print(p)
#h <- ggplot()
#h <- h + geom_point(aes(id, photoperiod), data = env_data, shape = 16, size = 5)
#View(h)

#histogram of photoperiod
ggplot(photoperiod_photoperiod, aes(x = photoperiod)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  labs(
    title = "Histogram of Photoperiod",
    x = "Photoperiod (minutes)",
    y = "Count"
  )
