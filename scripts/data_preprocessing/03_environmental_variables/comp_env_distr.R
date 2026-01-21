# Comparing the amount of detections in function of continious environmental variables (Tw, Q, Vw)
# and variables with a lower frequency (photoperiod)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

data_eels <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)
mydfnew.split.eel <- split(data_eels, data_eels$tag_serial_number) # split dataset based on tag IDs

metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

env_var <- "V" #Tw,V,...
metadata_filter <- filter(metadata, metadata$type == env_var)


distribution <- data.frame()
for (k in 1:length(mydfnew.split.eel)) {
  start <- min(mydfnew.split.eel[[k]]$arrival)
  end <- max(mydfnew.split.eel[[k]]$departure)
  for (i in 1:nrow(metadata_filter)) {
    path <- paste(
      "./data/interim/processed/",
      metadata_filter$name[i],
      "_",
      env_var,
      ".csv",
      sep = ""
    )
    temp <- read_csv(path, show_col_types = FALSE)
    temp <- temp %>%
      filter(Timestamp >= start & Timestamp <= end) %>%
      select(-starts_with("...")) #when reading in with read_csv column "...2" can be automatically created (columns not compatible then for rbind)
    temp$type <- "distribution"
    #temp$sample[which(temp$Timestamp %in% round_date(mydfnew.split.eel[[k]]$departure,unit=metadata_Tw$resolution[i]))] <- temp$Value[which(temp$Timestamp %in% round_date(mydfnew.split.eel[[k]]$departure,unit=metadata_Tw$resolution[i]))]

    #concat to previous temp data.frame
    distribution <- rbind(distribution, temp)
    id <- which(
      temp$Timestamp %in%
        round_date(
          mydfnew.split.eel[[k]]$departure,
          unit = metadata_filter$resolution[i]
        )
    )
    sample <- temp[id, ]
    sample$type <- "sample"
    distribution <- rbind(distribution, sample)
  }
}

p1 <- ggplot(distribution) +
  geom_density(
    aes(x = Value, colour = type, fill = type),
    alpha = 0.2,
    linewidth = 2,
    show.legend = TRUE
  ) +
  labs(
    title = paste("Density plot of", env_var, "values"),
    x = paste(env_var),
    y = "Density"
  ) +
  theme(
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  ) + #x as in logaritmic scale
  scale_x_log10()
# niet zo mooie gaussische curve
#ggsave("./figures/comparing_density_distributions/Q.png")

###############################################################################################################################################################
# photoperiod
distribution <- data.frame()
path <- "./data/interim/processed/photoperiod_photoperiod.csv"
temp <- read_csv(path, show_col_types = FALSE)
for (k in 1:length(mydfnew.split.eel)) {
  start <- min(mydfnew.split.eel[[k]]$arrival)
  end <- max(mydfnew.split.eel[[k]]$departure)
  temp_k <- temp %>% filter(Timestamp >= start & Timestamp <= end) #put timestamp column in POSIXct format
  temp_k$Timestamp <- as.POSIXct(temp_k$Timestamp, tz = "UTC")
  temp_k$type <- "distribution"
  #concat to previous temp data.frame
  distribution <- rbind(distribution, temp_k)
  id <- which(
    temp_k$Timestamp %in%
      round_date(mydfnew.split.eel[[k]]$arrival, unit = "day")
  )
  sample <- temp_k[id, ]
  sample$type <- "sample"
  distribution <- rbind(distribution, sample)
  #samples <- temp$Value[round_date(mydfnew.split.eel[[k]]$departure,unit=metadata_Tw$resolution[i])==temp$Timestamp,]
}

#density plot of distribution$Value
p2 <- ggplot(distribution) +
  geom_density(
    aes(x = Value, colour = type, fill = type),
    alpha = 0.2,
    linewidth = 2,
    show.legend = TRUE
  ) +
  labs(title = "Photoperiod", x = "Photoperiod", y = "Density") +
  theme(
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  ) + #x as in logaritmic scale
  scale_x_log10()
