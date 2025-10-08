library(tidyverse)
library(dplyr)
library(geosphere)
style <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(size = 30, colour = "black", angle = 90),
  axis.title.x = element_text(size = 30),
  axis.text.y = element_text(size = 30, colour = "black"),
  axis.title.y = element_text(size = 30),
  strip.text = element_text(size = 22), #title of facet wrap bigger
  legend.text = element_text(size = 27),
  legend.title = element_text(size = 30),
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
    arrival = ymd_hms(arrival, tz = "UTC", truncated = 3),
    departure = ymd_hms(departure, tz = "UTC", truncated = 3),
    date = ymd_hms(date, tz = "UTC", truncated = 3)
  ) %>%
  group_by(tag_serial_number) %>%
  filter(date > date[[1]] + days(1))

#raw
# data_env <- read_csv(
#   './data/interim/migration_env_filter.csv',
#   show_col_types = FALSE
# ) %>%
#   group_by(tag_serial_number) #%>%

##filter(arrival > arrival[[1]] + days(1)) #DIT WERKT NIET! zo valt het eerste
#datapunt volledig weg (dit is veel meer dan 1 dag dat je wegsmeet!!)

#als dat a temp een kolom data bevat

data_env <- data_env %>%
  filter(zone == "non-tidal") %>% #| zone == "transition") %>%
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
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE),
    year = year(arrival[1])
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

# load migration circadian
data_circadian <- read_csv(
  './data/interim/migration_circadian.csv',
  show_col_types = FALSE
) %>% #mutate(tag_serial_number = as.factor(tag_serial_number)) %>%
  dplyr::select(c(
    arrival_circadian,
    departure_circadian,
    tag_serial_number,
    arrival,
    departure
  ))

data <- left_join(
  data_env,
  data_circadian,
  by = c("tag_serial_number", "arrival", "departure")
)

######################################################
#Conditions
######################################################
variables <- c(
  "speed_m_s",
  "Q",
  #"delta_Q",#DELTA zegt niets bij raw data (delta over versch tijdspannes)
  "Tw",
  #"delta_Tw",
  #"S",aan als tidal data ook in rekening
  #"turb",
  #"O",
  "V",
  "photoperiod"
  #"speed_m_s",
  #"R",
  #"delta_R"
) #OR some individually: bv. "Q"

data_env_long <- data_env %>%
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(cluster)) %>% # filter out NA values %>%
  mutate(
    variable = factor(
      variable,
      levels = c("speed_m_s", "photoperiod", "Tw", "Q", "V")
    )
  )
y_labels <- c(
  speed_m_s = "Eel speed (m/s)",
  photoperiod = "Photoperiod (min)",
  Tw = "Temperature (°C)",
  Q = "Discharge (m³/s)",
  V = "Velocity (m/s)"
)
p <- ggplot(data_env_long, aes(x = label, y = value, fill = label)) + #choose the colors
  #scale_fill_manual(values = c("all" = "blue", "first_resident" = "red"), alpha = 0.5) +
  geom_boxplot(position = "dodge") +
  facet_wrap(
    ~variable,
    nrow = 1,
    scales = "free",
    labeller = as_labeller(y_labels)
  ) +
  style +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  windows(width = 16, height = 5)
plot(p)
# ggsave(
#   "./figures/Clustering/boxplot_labelled_non_tidal_inter.png",
#   height = 7,
#   width = 20
# )

######################################################
# Short-term trigger
######################################################
#making use of conditions in interpolated telemetry dataset (its about the Q the eel experieces)
variable <- "Q" #Tw?R?
delta_variable <- "delta_Q" #delta_Tw? delta_R?

list <- split(data_env, data_env$tag_serial_number) # lijst van alle eels
# identificeer het eerste knikpunt (tijdstip t_k) op plaats x_k ("migratory for the first time")
#empty dataframe
datatemp <- data.frame()

for (i in 1:length(list)) {
  data_eel <- list[[i]] # neem de eerste eel
  # identificeer het eerste knikpunt (tijdstip t_k)
  t_k <- data_eel$date[data_eel$first_migratory_idx[1]]
  # identificeer een range [t_k - r; t_k]
  range <- as.period(1, "days")
  # Interpoleer de Q's naar punt x_k (Q_{inter,k})
  Q_inter_k <- data_eel[[variable]]
  deltaQ_inter_k <- data_eel[[delta_variable]]

  #Bereken vergelijk de Q_max binnen de range met de Q_max binnen [t_release; t_k - r]
  Q_trigger <- max(Q_inter_k[
    data_eel$date > t_k - as.duration(range) &
      data_eel$date <= t_k
  ])
  Q_rest <- Q_inter_k[data_eel$date < t_k - as.duration(range)]

  deltaQ_trigger <- mean(deltaQ_inter_k[
    data_eel$date > t_k - as.duration(range) &
      data_eel$date <= t_k
  ])
  deltaQ_rest <- deltaQ_inter_k[data_eel$date < t_k - as.duration(range)]

  #create data frame: DELETE OR ADD DELTA DEPENDING ON PREFERENCE
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
  style + #other text on y axis, no x values in x  axis
  theme(axis.text.x = element_blank()) +
  labs(y = "Discharge (m³/s)", x = "Eel ID")
plot(g)
# ggsave(
#   "./figures/onset_of_migration/as_trigger/Q_rangemmax_1day_delday1.png",
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

######################################################
# Time of onset (MET ruwe data)
######################################################
data_onset <- data %>%
  group_by(tag_serial_number) %>%
  filter(row_number() == first_migratory_idx[1])

p <- ggplot(data_onset, aes(x = hour(departure))) +
  geom_bar(aes(fill = arrival_circadian)) + #aes(fill = arrival_circadian)
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  style +
  labs(title = "Onset of migration") + #remove legend
  guides(fill = guide_legend(title = "Circadian phase"))

p <- ggplot(data_onset, aes(x = hour(departure))) +
  geom_bar(aes(fill = arrival_circadian), color = "black") +
  coord_radial(r.axis.inside = TRUE, expand = FALSE) + # rotate so 0 is at north (start = -pi/120, direction = 1)
  scale_x_continuous(
    breaks = seq(0, 21, by = 3), # 0,3,6,9,12,15,18,21
    limits = c(0, 24) # ensure full circle
  ) +
  style +
  theme(axis.line = element_blank()) +
  labs(
    #title = "Onset of migration",
    x = "Hour of departure",
    y = element_blank()
  ) + #change position of label y axis
  guides(fill = guide_legend(title = "Circadian phase")) +
  annotate(
    "text",
    x = 24, # place at "north" outer edge
    y = max(table(hour(data_onset$departure))) * 0.5, # halfway up radial axis
    label = "# eels",
    angle = 90, # vertical orientation
    hjust = 0.4,
    vjust = 1.4,
    size = 11
  )
plot(p)
#save plot
ggsave(
  "./figures/onset_of_migration/onset_migration.png",
  width = 10,
  height = 10
)


######################################################
# GLMM Conditions
######################################################
library(glmm)

set.seed(1235)
data$label_bin <- NA

#glmm for onset
# data <- data %>%
#   group_by(tag_serial_number) %>%
#   filter(row_number() <= first_migratory_idx[1]) %>%
#   ungroup()
logic_scale <- TRUE
data_env <- data %>%
  mutate(
    label_bin = replace(label_bin, cluster == 1, 0),
    label_bin = replace(label_bin, cluster == 2, 1),
    tag_serial_number = as.factor(tag_serial_number),
    year = as.factor(year), # center V + Tw + Q + photoperiod + R
    Tw = scale(Tw, scale = logic_scale),
    photoperiod = scale(photoperiod, scale = logic_scale),
    Q = scale(Q, scale = logic_scale),
    V = scale(V, scale = logic_scale),
    R = scale(R, scale = logic_scale)
  ) %>%
  filter(!is.na(label_bin))

#Is the data scewed?
min_n <- min(table(data_env$label))
data_balanced <- data_env %>%
  group_by(label) %>%
  slice_sample(n = min_n, replace = TRUE) %>%
  ungroup()

table(data_balanced$label)
# glm
first_mod_glm <- glm(
  label_bin ~ Tw + photoperiod + arrival_circadian + Q + V,
  data = data_env,
  family = binomial
)

#check assumptions
#E(epsilon) = 0?
png("./figures/onset_of_migration/gl(m)m/glm_residuals.png")
g <- plot(first_mod_glm$fitted.values, first_mod_glm$residuals) +
  abline(h = 0, col = "red") +
  lines(
    loess.smooth(first_mod_glm$fitted.values, first_mod_glm$residuals),
    col = "blue",
    lwd = 2
  )
dev.off()

# glmm
mod_tag_year <- glmm(
  label_bin ~ V + Tw + Q + photoperiod + R,
  list(~ 0 + tag_serial_number, ~ 0 + year),
  varcomps.names = list("tag_serial_number", "year"),
  data = data_env,
  family = bernoulli.glmm,
  m = 10^4
)

clust <- makeCluster(4)
mod_tag <- glmm(
  label_bin ~ V + Tw + Q + photoperiod + R,
  ~ 0 + tag_serial_number,
  varcomps.names = "tag_serial_number",
  data = data_balanced,
  family = bernoulli.glmm,
  m = 10^5,
  cluster = clust
)
mod_year <- glmm(
  label_bin ~ V + Tw + Q + photoperiod + R,
  ~ 0 + year,
  varcomps.names = "year",
  data = data_env,
  family = bernoulli.glmm,
  m = 10^4
)


g <- plot(first_mod$fitted.values, first_mod$residuals) +
  abline(h = 0, col = "red") +
  lines(
    loess.smooth(first_mod$fitted.values, first_mod$residuals),
    col = "blue",
    lwd = 2
  )

#monte carlo size large enough for our needs?
#checking that the MC se are way smaller than the se of the coeffiecient estimates
#se(mod_tag) vs mcse(mod_tag)
#10^3 NOT LARGE ENOUGH

mod <- mod_tag
logLik_glmm <- logLik(mod)
n_params <- length(mod$beta) + length(mod$nu)
AIC_glmm <- -2 * as.numeric(logLik_glmm) + 2 * n_params
#keep only tag_serial_number as random effect

# OPMERKELIJK!
# HT COMBINEREN VAN VARIABELEN GEEFT EEN OMGEKEERDE RELATIE MET T_W
# HOGERE WAARDEN VAN T_W LEIDEN TOT EEN HOGERE KANS OP MIGRATIE
# IDEAL MIGRATIE SCENARIO = LAGER PHOTOPERIODE MAAR hoge TW --> dit zijn de condities waar de paling aan onderhevig is meer afwaarst in de rivier!

#onset of migration
clust <- makeCluster(4)
mod_tag <- glmm(
  label_bin ~ Tw + Q + R + photoperiod,
  ~ 0 + tag_serial_number,
  varcomps.names = "tag_serial_number",
  data = data_env,
  family = bernoulli.glmm,
  m = 10^5,
  cluster = clust
)

first_mod_glm <- glm(
  label_bin ~ Tw + Q + R + photoperiod,
  data = data_env,
  family = binomial
)
