# Comparing the amount of detections in function of the tidal rhythm (similar as in Keirsebelik et al. 2025)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

library(wateRinfo)
library(tidyr)
library(tidyverse)
library(patchwork)
library(fuzzyjoin)

#source functions
source('./src/arrival_departure_tides_function.R')
#Bij alle tidal measurement data:
#bnt07a-1066, bnt03a-1066, bnt01c-1066, BS-RUP-1096, zes28a-1066, zes21a-1066, zes14a-1066, zes10a-1066, zen01a-1066
#Tij bij kunnen zetten
#Tij data:
#niet BS-RUP-1096, rest wel

# read (meta)data and tide data
data_filter <- read_csv(
  './data/interim/migration_filter.csv',
  show_col_types = FALSE
)
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_tij <- filter(metadata, metadata$type == "tij")
n <- dim(metadata_tij)[1]

# add tij and interval columns
for (i in 1:n) {
  path <- paste('./data/raw/tide/', metadata_tij$name[i], '_tij.csv', sep = "")
  temp <- read_csv(path)
  temp$tij <- "HW"
  even_indexes <- seq(2, length(temp$Value), 2)
  oneven_indexes <- seq(1, length(temp$Value) - 1, 2)
  if ((temp$Value[1] - temp$Value[2]) > 0) {
    temp$tij[even_indexes] <- "LW"
  } else {
    temp$tij[oneven_indexes] <- "LW"
  }
  temp$interval <- temp$Timestamp %--% lead(temp$Timestamp)
  temp$interval_sec <- int_length(temp$interval)
  temp$name <- metadata_tij$name[i]
  assign(paste(metadata_tij$name[i], '_tij', sep = ""), temp)
}

# for (i in 1:n) {
#   path <- paste(
#     './data/interim/processed/',
#     metadata_tij$name[i],
#     '_tij.csv',
#     sep = ""
#   )
#   write.csv(get(paste(metadata_tij$name[i], '_tij', sep = "")), path)
# }

#########################################################################
# arrival departure analysis
#########################################################################
# data_filter_tij <- read_csv(
#   './data/interim/migration_filter_tides.csv',
#   show_col_types = FALSE
# )
p <- 1

data_filter_tij <- data_filter %>%
  mutate(
    tide_arrival = NA,
    tide_departure = NA,
    tidetime_arr = NA,
    tidetime_dep = NA,
    ebb_w = NA,
    flood_w = NA
  ) %>%
  filter(zone == "tidal")

#all tide data in a list
concat_tij <- lapply(
  metadata_tij$name,
  function(x) get(paste(x, '_tij', sep = ""))
)
#proportions for statistics
Proportion <- bind_rows(concat_tij) %>%
  group_by(name, tij) %>%
  summarise(
    sec = mean(interval_sec, na.rm = TRUE)
  ) %>%
  pivot_wider(names_from = tij, values_from = sec) %>%
  rename(
    flood = LW,
    ebb = HW
  ) %>% #to proportions
  mutate(
    flood_p = flood / (flood + ebb),
    ebb_p = ebb / (flood + ebb)
  )


for (i in 1:nrow(data_filter_tij)) {
  # choose the right tidal data
  idw_weights <- get_idx_weights(
    data_filter_tij[i, ],
    metadata_tij,
    dist_split
  )
  w <- idw_weights[[2]]
  selected_idx <- idw_weights[[1]]
  #get your weighted tide
  weighted <- get_weighted_tide(selected_idx, w, metadata_tij)

  #inverse dinstance rekenen met de tijdstippen van de tijdata
  ind_arr <- which(data_filter_tij$arrival[i] %within% weighted$interval)
  ind_dep <- which(data_filter_tij$departure[i] %within% weighted$interval)
  data_filter_tij$tidetime_arr[i] <- weighted$Timestamp[ind_arr]
  data_filter_tij$tidetime_dep[i] <- weighted$Timestamp[ind_dep]

  #Calculate proportions flood/ebb
  if (length(selected_idx) == 2) {
    ebb_1 <- Proportion$ebb_p[
      Proportion$name == metadata_tij$name[selected_idx[1]]
    ]
    ebb_2 <- Proportion$ebb_p[
      Proportion$name == metadata_tij$name[selected_idx[2]]
    ]
    flood_1 <- Proportion$flood_p[
      Proportion$name == metadata_tij$name[selected_idx[1]]
    ]
    flood_2 <- Proportion$flood_p[
      Proportion$name == metadata_tij$name[selected_idx[2]]
    ]
    data_filter_tij$ebb_w[i] <- (ebb_1 *
      w[selected_idx[1]] +
      ebb_2 * w[selected_idx[2]]) /
      (sum(w[selected_idx]))

    data_filter_tij$flood_w[i] <- (flood_1 *
      w[selected_idx[1]] +
      flood_2 * w[selected_idx[2]]) /
      (sum(w[selected_idx]))
  } else {
    ind <- selected_idx
    data_filter_tij$ebb_w[i] <- Proportion$ebb_p[
      Proportion$name == metadata_tij$name[ind]
    ]
    data_filter_tij$flood_w[i] <- Proportion$flood_p[
      Proportion$name == metadata_tij$name[ind]
    ]
  }

  #calculate tij
  if (weighted$tij[ind_arr] == "HW") {
    data_filter_tij$tide_arrival[i] <- "ebb"
    data_filter_tij$tidetime_arr[i] <- interval(
      start = weighted$Timestamp[ind_arr],
      end = data_filter_tij$arrival[i]
    )
  } else {
    data_filter_tij$tide_arrival[i] <- "flood"
    data_filter_tij$tidetime_arr[i] <- interval(
      start = weighted$Timestamp[ind_arr - 1],
      end = data_filter_tij$arrival[i]
    )
  }
  if (weighted$tij[ind_dep] == "HW") {
    data_filter_tij$tide_departure[i] <- "ebb"
    data_filter_tij$tidetime_dep[i] <- interval(
      start = weighted$Timestamp[ind_dep],
      end = data_filter_tij$departure[i]
    )
  } else {
    data_filter_tij$tide_departure[i] <- "flood"
    data_filter_tij$tidetime_dep[i] <- interval(
      start = weighted$Timestamp[ind_dep - 1],
      end = data_filter_tij$departure[i]
    )
  }
}


#van HW --> HW is +- 12u dus verdeling per halfuur ongeveer
data_filter_tij <- read_csv(
  './data/interim/migration_filter_tides.csv',
  show_col_types = FALSE
)
p1 <- ggplot(data_filter_tij, aes(x = hour(tidetime_arr))) + #hier hoever van hoog en laag tij
  geom_bar(aes(fill = tide_arrival), width = ) +
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  labs(title = "Arrivals at receicers") +
  theme(legend.position = "none") + #axis.title.x = element_text(size = 16)
  xlab("Hours after high water")

p2 <- ggplot(data_filter_tij, aes(x = hour(tidetime_dep))) + #hier hoever van hoog en laag tij
  geom_bar(aes(fill = tide_departure)) + #choose colors for fill

  coord_radial(r.axis.inside = TRUE, expand = TRUE, start = pi/2) +
  scale_x_continuous(limits = c(0, 12), breaks = 0:12, expand = c(0, 0)) +
  labs(fill = "Tide:", y = NULL, x = "Hours after high water") +
  scale_fill_manual(values = c("ebb" = grey1, "flood" = "black")) +
  style +
  theme(
    #axis.line = element_blank(),
    strip.text = element_text(size = 25),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey90"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)
  ) +
  annotate(
    "text",
    x = 12, # place at "north" outer edge
    y = max(table(hour(data_filter_tij$tidetime_dep))) * 0.5, # halfway up radial axis
    label = "# obs",
    #angle = 90, # vertical orientation
    hjust = 0.4,
    vjust = 1.4,
    size = 11
  )
print(p2)
ggsave("./figures/Tide/tidal_zone_dep_bw.png", p2, width = 7, height = 7)

write_csv(
  data_filter_tij,
  './data/interim/migration_filter_tides.csv',
  col_names = TRUE
)
#######################################################################
# statistical analysis (chi-squared test)
#######################################################################
# assumption: flood is less abundantly present than ebb, so we need to coorect for this --> in data_filter_tij
#heb ik voor alle detecties de proportie flood/ebb berekend door rekening te houden met de proportie flood/ebb in het dichtst
#bijzijnde tij meetpunt
Counts <- data_filter_tij %>%
  group_by(zone, tide_departure) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = zone, values_from = count, values_fill = 0)
prop_sum <- c(
  mean(data_filter_tij$ebb_w, na.rm = TRUE),
  mean(data_filter_tij$flood_w, na.rm = TRUE)
)

chi_test <- chisq.test(x = Counts$tidal, p = prop_sum)
chi_test$p.value
#illustration of proportion flood/ebb
ggplot(bnt01c_1066_tij) +
  geom_boxplot(aes(x = tij, y = interval_sec))
