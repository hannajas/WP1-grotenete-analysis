library(tidyverse)
library(dplyr)
style <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(size = 20, colour = "black", angle = 90),
  axis.title.x = element_text(size = 25),
  axis.text.y = element_text(size = 25, colour = "black"),
  axis.title.y = element_text(size = 25)
)

########################################################
#make selection in the data
########################################################
#non-tidal
#delete day one
#fill 'resident' and 'migration'
#leave out the eels without onset (1294169, 1305785)
#leave out eels with only one detection (1171747, 1171751, 1294168, 1294172)

# load and process data
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#inter
data_env <- read.csv(
  "./data/interim/migration_env_inter.csv",
  header = TRUE,
  sep = ","
) %>%
  mutate(
    arrival = as.POSIXct(arrival, tz = "UTC", truncated = 3),
    departure = as.POSIXct(departure, tz = "UTC", truncated = 3),
    date = as.POSIXct(date, tz = "UTC", truncated = 3)
  ) %>%
  group_by(tag_serial_number) %>%
  filter(date > date[[1]] + days(1))

#raw
# data_env <- read_csv(
#   './data/interim/migration_env_filter.csv',
#   show_col_types = FALSE
# ) %>%
#   group_by(tag_serial_number) %>%
#   filter(arrival > arrival[[1]] + days(1))

#als data temp een kolom data bevat

data_env <- data_env %>%
  filter(zone == "non-tidal") %>% # | zone == "transition"
  filter(
    !tag_serial_number %in%
      c(1294169, 1305785, 1171747, 1171751, 1294168, 1294172)
  ) %>%
  mutate(
    label = ifelse(
      cluster == 1,
      "resident/resting",
      ifelse(cluster == 2, "migratory", NA)
    )
  ) %>%
  mutate(
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE)
  ) %>%
  mutate(
    label = case_when(
      label == "resident/resting" &
        row_number() < first_migratory_idx ~
        "resident",
      label == "resident/resting" &
        row_number() > first_migratory_idx ~
        "migration",
      label == "migratory" ~ "migration",
      is.infinite(first_migratory_idx) | is.na(first_migratory_idx) ~ "resident"
    )
  ) %>%
  ungroup()

######################################################
#Conditions
######################################################
variables <- c(
  #"logQ",
  "delta_Q",
  "Tw",
  "delta_Tw",
  #"S",aan als tidal data ook in rekening
  #"turb",
  #"O",
  "V",
  "photoperiod",
  "speed_m_s"
) #OR some individually: bv. "Q"

data_env_long <- data_env %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(cluster)) # filter out NA values

p <- ggplot(data_env_long, aes(x = label, y = value, fill = label)) + #choose the colors
  #scale_fill_manual(values = c("all" = "blue", "first_resident" = "red"), alpha = 0.5) +
  geom_boxplot(position = "dodge") +
  facet_wrap(~variable, nrow = 1, scales = "free") +
  style +
  theme(legend.position = "none")
windows(width = 16, height = 5)
plot(p)
#ggsave("./figures/Clustering/boxplot_labelled_non_tidal.png", height = 10, width = 20)

######################################################
#Short-term trigger
######################################################
variable <- "Q"

# source functions
source("./src/align_resolutions_function.R")
source("./src/inverse_distance_function.R")

list <- split(data_env, data_env$tag_serial_number) # lijst van alle eels
# identificeer het eerste knikpunt (tijdstip t_k) op plaats x_k ("migratory for the first time")
#empty dataframe
datatemp <- data.frame()

for (i in 1:length(list)) {
  data_eel <- list[[i]] # neem de eerste eel
  # #error message if to little detection points
  # data_eel$date <- as.POSIXct(data_eel$date, tz = "UTC") # zorg dat de datum in het juiste formaat is
  # index <- which(data_eel$cluster == 2)[1] # index van het eerste knikpunt
  # if (nrow(data_eel) < 2) {
  #   next
  #   #stop("Not enough detection points for this eel.")
  # } else if (index == 1 | is.na(index)) {
  #   next
  #   #stop("Eel started as migratory.")
  # }
  t_k <- data_eel$date[data_eel$first_migratory_idx[1]]
  x_k <- data_eel$distance_to_source_m[data_eel$first_migratory_idx[1]]

  # identificeer een range [t_k - r; t_k]
  range <- as.period(1, "days")
  # Interpoleer de Q's naar punt x_k (Q_{inter,k})
  #data_eel$distance_to_source_m <- x_k #DUS dit is super schaalbar naar v!!

  env_data_Q <- align_resolutions_function(
    variable,
    as.difftime(5, units = "mins"), #as.period(5, "mins"),
    metadata,
    upsample_method = "ffill",
    data_eel
  )
  Q_inter_k <- inverse_distance(
    data_eel,
    env_data_Q,
    metadata,
    1,
    variable
  )
  deltaQ_inter_k <- Q_inter_k - lag(Q_inter_k)

  #proberen met V ipv Q
  # Q_inter_k <- read_csv(
  #   "./data/interim/processed/gnt07a_1066_V.csv"
  # )

  #Bereken vergelijk de Q_max binnen de range met de Q_max binnen [t_release; t_k - r]
  Q_trigger <- max(Q_inter_k[
    data_eel$date > t_k - as.duration(range) &
      data_eel$date <= t_k
  ])
  Q_rest <- Q_inter_k[data_eel$date < t_k - as.duration(range)]

  # deltaQ_trigger <- mean(deltaQ_inter_k[
  #   data_eel$date > t_k - as.duration(range) &
  #     data_eel$date <= t_k
  # ])
  # deltaQ_rest <- deltaQ_inter_k[data_eel$date < t_k - as.duration(range)]

  #create data frame
  data_plot <- data.frame(
    Q = c(Q_rest, Q_trigger),
    tag_serial_number = data_eel$tag_serial_number[1],
    type = c(
      rep("Q_rest", length(Q_rest)),
      rep("trigger", length(Q_trigger))
    )
  )
  #marge with datatemp
  datatemp <- rbind(datatemp, data_plot)
}

dataplot <- datatemp[datatemp$type == "Q_rest", ]
trigger_vals <- datatemp[
  datatemp$type == "trigger",
  c("tag_serial_number", "Q")
]
#boxplot Qrest and plot Q_trigger with ggplot2
g <- ggplot(data = dataplot) +
  geom_boxplot(aes(y = Q)) +
  geom_hline(
    data = trigger_vals,
    aes(yintercept = Q, group = tag_serial_number),
    color = "red"
  ) +
  facet_wrap(~tag_serial_number) +
  style + #other text on y axis
  labs(y = "Q (m^3/s)")
plot(g)
# ggsave(
#   "./figures/as_trigger/Q_rangemax_1day_delday1.png",
#   plot = g,
# )

#plot Q_trigger
# p <- ggplot() +
#   geom_line(aes(x = data_eel$date, y = Q_inter_k), color = "blue") +
#   geom_vline(xintercept = t_k, linetype = "dashed", color = "red") +
#   labs(
#     title = paste("Q values for", data_eel$tag_serial_number[1]),
#     x = "Date",
#     y = "Q values"
#   ) +
#   theme_minimal()
