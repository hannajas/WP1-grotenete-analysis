library(tidyverse)
library(dplyr)
library(ragg)
data_filter <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)
# Plot style
style <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(size = 20, colour = "black", angle = 90),
  axis.title.x = element_text(size = 25),
  axis.text.y = element_text(size = 25, colour = "black"),
  axis.title.y = element_text(size = 25)
)

########################################################################################################
# CORRELATION PLOTS
########################################################################################################

########################################################################################################
# Tw
p <- ggplot() +
  geom_point(
    aes(Tw, speed_m_s, colour = downstream_migration),
    data = data_filter,
    shape = 16,
    size = 5
  ) +
  style +
  labs(x = "Tw [m^3/s]", y = "speed_m_s")
#ggsave('./figures/correlations/watertemperature.png')

d1 <- ggplot() +
  geom_point(
    aes(delta_Tw, speed_m_s),
    data = data_filter,
    shape = 16,
    size = 5
  ) +
  style
#ggsave('./figures/correlations/delta_watertemperature.png')

########################################################################################################
# Q
p1 <- ggplot(data_filter) +
  geom_point(
    aes(Q, downstream_migration, colour = downstream_migration),
    shape = 16,
    size = 5
  ) +
  style +
  labs(x = "Debiet [m^3/s]", y = "migration")


Q_delta <- ggplot(data_filter, aes(delta_Q, speed_m_s)) +
  geom_point(shape = 16, size = 5) +
  style

########################################################################################################
#water velocity
# based on datafilter calculate for each day the weighted average of speed_m_s (weigthed on the period of the day that this speed_was measured)
data_filter <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)
data_filter$middledate <- as.POSIXct(round_date(
  data_filter$arrival + data_filter$residence / 2,
  "15 mins"
))

data_filter <- data_filter %>%
  mutate(
    midnight = floor_date(middledate, "day") %within%
      (middledate %--% lag(middledate))
  )
data_filter$weight <- NA
for (i in 2:nrow(data_filter)) {
  if (data_filter$midnight[i] == TRUE) {
    data_filter$weight[i] <- int_length(
      data_filter$middledate[i] %--%
        floor_date(data_filter$middledate[i], "day")
    ) # nog mee rekeing houden als de paling er meer dan 1 DAG OVER DOET
  } else {
    data_filter$weight[i] <- int_length(
      data_filter$middledate[i] %--% data_filter$middledate[i - 1]
    )
  }
}

data_eels <- data_filter %>%
  filter(tag_serial_number == 1171746)

data_day <- data.frame(
  date = seq(
    floor_date(data_eels$arrival[1], "day"),
    floor_date(data_eels$arrival[nrow(data_eels)], "day"),
    by = "days"
  ),
  speed_m_s = NA
)

for (i in 1:nrow(data_day)) {
  data_day_filter <- data_eels %>%
    filter(
      floor_date(arrival, "day") == data_day$date[i] |
        floor_date(departure, "day") == data_day$date[i]
    )
  if (nrow(data_day_filter) > 1) {}
}

data_filter <- data_filter %>%
  filter(zone == "non-tidal")
p1 <- ggplot(data_filter) +
  geom_point(aes(V, migration_speed), shape = 16, size = 5) +
  style
#ggsave('./figures/correlations/water_velocity.png')

########################################################################################################
#photoperiod
d3 <- ggplot() +
  geom_point(
    aes(photoperiod, speed_m_s),
    data = data_filter,
    shape = 16,
    size = 5
  ) +
  style +
  labs(x = "photoperiod [min]", y = "speed_m_s") +
  xlim(460, 700)

d2 <- ggplot()
d2 <- d2 +
  geom_point(
    aes(downstream_migration, photoperiod),
    data = data_filter,
    shape = 16,
    size = 5
  )
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle = 90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25)
  )
d2 <- d2 + labs(x = "photoperiod [min]", y = "migration")
# ggsave('./figures/photoperiod_migration.png')

d2 <- ggplot()
d2 <- d2 +
  geom_point(
    aes(delta_photoperiod, speed_m_s),
    data = data_filter,
    shape = 16,
    size = 5
  )
d2 <- d2 +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 20, colour = "black", angle = 90),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25, colour = "black"),
    axis.title.y = element_text(size = 25)
  )
d2 <- d2 + xlim(-5, 5)

d2 <- d2 + labs(x = "photoperiod [min]", y = "migration")


########################################################################################################
# RAINFALL
R1 <- ggplot(data_filter, aes(R, speed_m_s)) +
  geom_point(shape = 16, size = 5) +
  style +
  labs(x = "R [mm]", y = "speed_m_s")

########################################################################################################
# SALINITY
grenswaardes_distance_to_source <- c(43106.96, 70306.3)
data_filter <- filter(
  data,
  !startsWith(data$station_name, "ws-") & (data$distance_to_source_m > 70306.3)
) #+- 62 waarden uitgelaten

S_zes <- ggplot(data_filter, aes(S, speed_m_s)) +
  geom_point(shape = 16, size = 5) +
  style +
  labs(x = "S [mm]", y = "speed_m_s")
#ggsave('./figures/correlations/salinity.png')

########################################################################################################
# TURBIDITY
data_filter <- filter(data, !startsWith(data$station_name, "ws-"))

T1 <- ggplot(data_filter, aes(turb, migration_speed)) +
  geom_point(shape = 16, size = 5) + #geom_smooth(method = lm,formula = y ~ log(x))+
  style +
  labs(x = "turb [NTU]", y = "speed_m_s")
#ggsave('./figures/correlations/salinity.png')

########################################################################################################
# OXYGEN
data_filter <- filter(data, !startsWith(data$station_name, "ws-"))

O1 <- ggplot(data_filter, aes(O, speed_m_s)) +
  geom_point(shape = 16, size = 5) + #geom_smooth(method = lm,formula = y ~ log(x))+
  style +
  labs(x = "O [mg/l]", y = "speed_m_s") +
  xlim(4, 11)


#######################################################################################################################

# PLOTTING EACH SEGMENT
# Bv. voor traject gn-13 tot gn-11
data_sort <- data_filter[order(data_filter$distance_to_source_m), ]
#datasort <- sort(data_filter$distance_to_source_m)
stations <- unique(data_sort$station_name)
# skip stations in Scheldt more upstream than more upstream than Rupel
seq_af <- stations[
  stations != "s-7" &
    stations != "s-6" &
    stations != "s-5" &
    stations != "s-4c" &
    stations != "s-4b" &
    stations != "s-4a"
]
#skip release location 3 en 2
seq_af <- seq_af[seq_af != "rel_grotenete2" & seq_af != "rel_grotenete3"]


# plot the speed of the fish for each segment
pdf("./figures/Downstream_segments_Tw_Q.pdf") # Create pdf
for (i in 2:length(seq_af) - 1) {
  data_temp <- filter(
    data_filter,
    dplyr::lag(station_name) == seq_af[i] & station_name == seq_af[i + 1]
  )
  print(paste("data_temp:", dim(data_temp)[1]))
  p1 <- ggplot(data_temp, aes(Tw, speed_m_s)) +
    geom_point(shape = 16, size = 5) +
    geom_smooth(method = lm, size = 2) +
    style +
    labs(x = "Temperature [°C]", y = "speed_m_s")

  p2 <- ggplot(data_temp, aes(Q, speed_m_s)) +
    geom_point(shape = 16, size = 5) +
    geom_smooth(method = lm, linewidth = 2) +
    style +
    labs(x = "Discharge [m^3/s]", y = "speed_m_s")
  p2 <- p2 + labs(title = paste("From segment", seq_af[i], "to", seq_af[i + 1]))
  print(p2 / p1 + plot_layout(heights = c(1, 1)))
  #print(p1)
}
dev.off()

# welk soort traject wordt he meeste getraceerd?

gn13_11 <- filter(
  data_filter,
  dplyr::lag(station_name) == "gn-13" & station_name == "gn-11"
)
p <- ggplot(gn13_11, aes(Tw, speed_m_s)) +
  geom_point(shape = 16, size = 5) +
  geom_smooth(method = lm, size = 2) +
  style +
  labs(x = "Debiet [m^3/s]", y = "speed_m_s")

gn9_7 <- filter(
  data_filter,
  dplyr::lag(station_name) == "gn-9" & station_name == "gn-7"
)
p <- ggplot(gn9_7, aes(Q, speed_m_s)) +
  geom_point(shape = 16, size = 5) +
  geom_smooth(method = lm, size = 2) +
  style +
  labs(x = "Debiet [m^3/s]", y = "speed_m_s")


###########################################################################################################################
# Correlation between turbidity and river flow
rup02e_SF_1066_turb <- read_csv(
  './data/interim/processed/rup02e_SF_1066_turb.csv',
  show_col_types = FALSE
)

rup00a_1066_Q <- read_csv(
  './data/interim/processed/rup00a_1066_Q.csv',
  show_col_types = FALSE
)
turb_Q_rup <- left_join(rup02e_SF_1066_turb, rup00a_1066_Q, "Timestamp")
g <- ggplot(turb_Q_rup) +
  theme(
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    axis.text.y.right = element_text(color = "blue")
  ) +
  geom_line(aes(Timestamp, Value.x), linewidth = 1) +
  geom_point(aes(Timestamp, Value.y * 1), colour = "blue", size = 6) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 1, name = "Discharge (m³/s)")) +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 20)) +
  #labs(title = id_unique[a])+
  ylab("Turbidity (NTU)") +
  xlab("Date")
print(g)
ggsave(
  g,
  filename = "C:/Code/WP1-grotenete-analysis/figures/Q_en_turb.png",
  height = 10,
  width = 20
)


ggplot(Q_turb_rup, aes(Q, Turbidity)) +
  geom_point(shape = 16, size = 5) +
  style +
  labs(x = "Q [m^3/s]", y = "Turbidity [NTU]")


# correlations between environmental variables
Q_turb_rup <- rup00a_1066_Q %>%
  rename(Q = Value)
variables <- c(
  "rup02e_SF_1066_turb",
  "rup02e_SF_1066_S",
  "rup02e_SF_1066_O",
  "rup02e_SF_1066_Tw",
  "P04_027_R"
)
for (i in variables) {
  env_data <- read_csv(
    paste0('./data/interim/processed/', i, '.csv'),
    show_col_types = FALSE
  )
  Q_turb_rup$temp <- unlist(lapply(1:nrow(Q_turb_rup), function(a) {
    mean(env_data$Value[
      env_data$Timestamp >= Q_turb_rup$Timestamp[a] &
        env_data$Timestamp < Q_turb_rup$Timestamp[a] + ddays(x = 1)
    ])
  }))
  names(Q_turb_rup)[dim(Q_turb_rup)[2]] <- i
}


# pair plot
p <- Q_turb_rup %>%
  select(Q, variables) %>%
  pairs

#correlation matrix
p <- Q_turb_rup %>%
  select(Q, variables) %>%
  GGally::ggpairs()


##############################################################################################
#correlation between raw env data
env_data <- read_csv(
  './data/interim/processed/rup02e_SF_1066_turb.csv',
  show_col_types = FALSE
) %>%
  rename(rup02e_SF_1066_turb = Value)

variables <- c("rup02e_SF_1066_S", "rup02e_SF_1066_O", "rup02e_SF_1066_Tw")
for (i in variables) {
  env_data_temp <- read_csv(
    paste0('./data/interim/processed/', i, '.csv'),
    show_col_types = FALSE
  )
  env_data$temp <- env_data_temp$Value
  names(env_data)[dim(env_data)[2]] <- i
}
p <- env_data %>%
  dplyr::select(rup02e_SF_1066_turb, variables) %>%
  GGally::ggpairs()


##############################################################################################
# AFTER CLUSTERING
##############################################################################################
variables <- c(
  "logQ",
  "delta_Q",
  "Tw",
  "delta_Tw",
  "S",
  "turb",
  "O",
  "V",
  "photoperiod",
  "speed_m_s"
) #OR some individually: bv. "Q"
data_cluster <- read_csv(
  './data/interim/migration_env_filter_kmeans.csv',
  show_col_types = FALSE
)
data_cluster <- data_cluster %>%
  mutate(cluster = as.factor(cluster), logQ = log(Q))
data_cluster$label <- recode(
  data_cluster$cluster,
  `1` = "resident",
  `2` = "migratory"
)

#make a column 'keep' to keep only the first part of the resident phase
only_first_resident <- data_cluster %>%
  group_by(tag_serial_number) %>%
  mutate(
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE)
  ) %>%
  mutate(
    keep = case_when(
      label == "migratory" ~ TRUE,
      label == "resident" &
        is.finite(first_migratory_idx) &
        row_number() < first_migratory_idx ~
        TRUE,
      label == "resident" & !is.finite(first_migratory_idx) ~ TRUE, # keep all residents if no migratory part
      TRUE ~ FALSE
    )
  ) %>%
  ungroup() %>%
  filter(keep)

only_first_resident_long <- only_first_resident %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(cluster)) # filter out NA values

data_cluster_long <- data_cluster %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(cluster)) # filter out NA values

#SCATTERPLOT
#variable to plot - EDITABLE
#other options then Q are "logQ", "delta_Q", "Tw", "delta_Tw", "S", "turb", "O", "V", "photoperiod"
p1 <- ggplot(data_cluster[data_cluster$keep, ]) +
  geom_point(aes(Q, speed_m_s, colour = cluster), shape = 16, size = 5) + #log on y axis
  scale_x_log10(guide = "axis_logticks") +
  style #theme settings in beginning of the script


#BOXPLOT
# Add source column, but set migratory always to "all"
data_cluster_long$source <- "all data"
only_first_resident_long$source <- ifelse(
  only_first_resident_long$label == "migratory",
  "all data",
  "first resident"
)

data_combined <- bind_rows(data_cluster_long, only_first_resident_long)

# Remove duplicate migratory rows from only_first_resident_long
data_combined <- data_combined %>%
  distinct(label, value, variable, source, .keep_all = TRUE) #als je zelfde datarij hebt en er staat ook 2 keer "all data" dan heb je een migratory van only_first_resident en data_cluster --> verwijder er 1

# Plot
p <- ggplot(data_combined, aes(x = label, y = value, fill = source)) + #choose the colors
  #scale_fill_manual(values = c("all" = "blue", "first_resident" = "red"), alpha = 0.5) +
  geom_boxplot(position = "dodge") +
  facet_wrap(~variable, nrow = 1, scales = "free") +
  style
windows(width = 16, height = 5)
plot(p)

# with the interpolated data
#load rds file
data_interpolated <- readRDS(
  './data/interim/migration_env_filter_kmeans_long.rds'
)