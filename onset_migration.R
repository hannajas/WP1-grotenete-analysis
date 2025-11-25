library(tidyverse)
library(dplyr)
library(geosphere)
library(ggplot2)
library(themis) #package to deal with unbalanced data
library(lubridate)
library(lme4)
library(glmmTMB)
library(DHARMa)

########################################################
#make selection in the data
########################################################
#non-tidal
#delete day one
#fill 'resident' and 'migration'
#leave out the eels without onset (1294169, 1305785)
#leave out eels with only one detection (1171747, 1171751, 1294168, 1294172)
# fix variables
true_scale <- TRUE

# load and process data
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#inter
# data_env <- read.csv(
#   "./data/interim/migration_env_inter.csv",
#   header = TRUE,
#   sep = ","
# ) %>%
#   mutate(
#     arrival = ymd_hms(arrival, tz = "UTC", truncated = 3),
#     departure = ymd_hms(departure, tz = "UTC", truncated = 3),
#     date = ymd_hms(date, tz = "UTC", truncated = 3)
#   ) %>%
#   group_by(tag_serial_number) %>%
#   filter(date > date[[1]] + days(1))

#raw
data_raw <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  group_by(tag_serial_number) #%>%

##filter(arrival > arrival[[1]] + days(1)) #DIT WERKT NIET! zo valt het eerste
#datapunt volledig weg (dit is veel meer dan 1 dag dat je wegsmeet!!)

#als dat a temp een kolom data bevat
no_detections <- c(1171747, 1171751, 1294168, 1294172)
no_onset <- c(
  1294169,
  1305785,
  1293822,
  1294165,
  1294167,
  1294170,
  1294189,
  1294191,
  1305786,
  1305788,
  1305791
) #1305791 never migratory, rest never resident
data_env <- data_raw %>%
  filter(zone == "non-tidal") %>% #| zone == "transition") %>%
  filter(
    !tag_serial_number %in%
      c(no_onset, no_detections)
  ) %>%
  #method PJ for onset migration
  # mutate(
  #   label = ifelse(
  #     migration == FALSE,
  #     "resident",
  #     ifelse(migration == TRUE, "migration", NA)
  #   )
  # ) %>%
  # mutate(
  #   label = case_when(
  #     label == "migration" &
  #       cluster == 1 ~
  #       "migration",
  #     label == "migration" &
  #       cluster == 2 ~
  #       "migration",
  #     label == "resident" ~ "resident"
  #   )
  # ) %>%
  mutate(
    label = ifelse(
      cluster == 1,
      "resident/resting",
      ifelse(cluster == 2, "migratory", NA)
    )
  ) %>%
  mutate(
    first_migratory_idx = min(which(label == "migratory"), na.rm = TRUE),
    year = year(arrival[1]),
    month = month(arrival[1]),
    month_onset = month(arrival[first_migratory_idx[1]]),
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
#data_env$R <- log(data_env$R)
data_env %>%
  dplyr::select(R, cluster) %>%
  group_by(cluster) %>%
  summarise(mean_R = mean(R, na.rm = TRUE), median_R = median(R, na.rm = TRUE))

variables <- c(
  "speed_m_s",
  "Q",
  #"Q_an",
  #"delta_Q",#DELTA zegt niets bij raw data (delta over versch tijdspannes)
  "Tw",
  #"Tw_an",
  #"delta_Tw",
  #"S",aan als tidal data ook in rekening
  #"turb",
  #"O",
  "V",
  "photoperiod",
  #"speed_m_s",
  "R"
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
      levels = variables
    )
  )
# y_labels <- c(
#   speed_m_s = "Eel speed (m/s)",
#   photoperiod = "Photoperiod (min)",
#   Tw = desparse(bquote(~(T_w)"(°C)")),
#   Tw_an = "Temperature anomaly (°C)",
#   Q = "Discharge (-)",
#   Q_an = "Discharge anomaly (m³/s)",
#   V = "Velocity (m/s)",
#   R = "Rainfall (mm)"
# )
p <- ggplot(data_env_long, aes(x = label, y = value, fill = label)) + #choose the colors
  #scale_fill_manual(values = c("all" = "blue", "first_resident" = "red"), alpha = 0.5) +
  geom_boxplot(position = "dodge") +
  facet_wrap(
    ~variable,
    nrow = 1,
    scales = "free",
    labeller = as_labeller(
      c(
        "speed_m_s" = "v~\"(m/s)\"",
        "photoperiod" = "P~\"(min)\"",
        "Tw" = "T[w]~\"(°C)\"",
        "Q" = "Q[scaled]~\"(-)\"",
        "V" = "V[w]~\"(m/s)\"",
        "R" = "R~\"(mm)\""
      ),
      label_parsed
    )
  ) +
  style +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 18)
  ) +
  windows(width = 16, height = 5)
plot(p)
ggsave(
  "./figures/Clustering/boxplot_labelled_non_tidal_Qscaled.png"
)

######################################################
# Short-term trigger - INTERPOLATED DATA
######################################################
#making use of conditions in interpolated telemetry dataset (its about the Q the eel experieces)
variable <- "Q" #Tw?R?
delta_variable <- "delta_Q" #delta_Tw? delta_R?

list <- split(data_env, data_env$tag_serial_number) # lijst van alle eels
# identificeer het eerste knikpunt (tijdstip t_k) op plaats x_k ("migratory for the first time")
#empty dataframe
datatemp <- data.frame()
# identificeer een range [t_k - r; t_k]
range <- as.period(1, "days")

for (i in 1:length(list)) {
  data_eel <- list[[i]] # neem de eerste eel
  # identificeer het eerste knikpunt (tijdstip t_k)
  #t_k <- data_eel$date[data_eel$first_migratory_idx[1]]
  t_k <- data_eel$date[which(data_eel$label == "migration")[1]]
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
ggsave(
  "./figures/onset_of_migration/as_trigger/Q_rangemmax_1day_delday1_new.png",
  plot = g,
)

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
  theme(axis.line = element_blank(), strip.text = element_text(size = 25)) +
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


############################################################################################################
# GLMM Conditions
# How are the environmental conditions different from an eel that started migration VS an eel still resident
# Is already in the question timing and place is partly what divides the classes
# Miss is GLMM daarom nietzoveel zeggend?
#gaan we er nietvanuit (expert knoledge dat er )
############################################################################################################
library(glmm)
set.seed(1234)

######################################################
#Prepare data
data$label_bin <- NA
true_scale <- TRUE

#use accumulated R
# data <- data %>%
#   group_by(tag_serial_number) %>%
#   mutate(R = cumsum(replace_na(R, 0))) %>%
#   ungroup()

#scale data
data_env <- data %>%
  mutate(
    label_bin = replace(label_bin, label == "resident", 0),
    label_bin = replace(label_bin, label == "migration", 1),
    # label_bin = replace(label_bin, cluster == 1, 0), #resident = 0
    # label_bin = replace(label_bin, cluster == 2, 1), #migration = 1
    tag_serial_number = as.factor(tag_serial_number),
    year = as.factor(year), # center V + Tw + Q + photoperiod + R
    month = as.factor(month),
    Tw = scale(Tw, scale = true_scale),
    Tw_an = scale(Tw_an, scale = true_scale),
    photoperiod = scale(photoperiod, scale = true_scale),
    Q = scale(Q, scale = true_scale),
    Q_an = scale(Q_an, scale = true_scale),
    V = scale(V, scale = true_scale),
    R = scale(R, scale = true_scale),
    distance_to_source_m = scale(distance_to_source_m, scale = true_scale)
  ) %>%
  filter(!is.na(label_bin)) %>%
  group_by(tag_serial_number) %>%
  mutate(time = as.factor(as.numeric(row_number()))) %>%
  ungroup()


######################################################
#Analyse the data

#Is the data skewed?
skew_table <- table(data_env$label)

table(data_balanced$label)

#Is the data correlated
#correlogram
cols <- as.factor(data_env$label)
palette <- c("resident" = "blue", "migration" = "red")
col_vec <- palette[as.character(cols)]

pairs(
  data_env %>% dplyr::select(Tw_an, Q_an, Q, Tw, photoperiod, R),
  col = adjustcolor(col_vec, alpha.f = 0.5),
  pch = 22
)

#correlation matrix
cor_mat <- data_env %>%
  dplyr::select(Tw_an, Tw, Q_an, Q, photoperiod, R) %>%
  cor(use = "pairwise.complete.obs")

ggplot(data_env, aes(x = V, y = Tw, color = label)) +
  geom_point()


######################################################
# RANDOM UNDERSAMPLING
min_n <- min(skew_table)
data_balanced <- data_env %>%
  group_by(label) %>%
  slice_sample(n = min_n, replace = FALSE) %>%
  ungroup()

#correlogram balanced
pairs(
  data_balanced %>% dplyr::select(Tw, Q, V, photoperiod, R),
  col = adjustcolor(col_vec, alpha.f = 0.5),
  pch = 22
)

#correlation matrix
cor_mat <- data_balanced %>%
  dplyr::select(Tw, Q, V, photoperiod, R) %>%
  cor(use = "pairwise.complete.obs")
#STILL HIGHLY correlation
#Enkel nog Tw en Q houden???

#SMOTE (OVERSAMPLING)
arrival_circadian_fac <- relevel(
  as.factor(data_env$arrival_circadian),
  ref = "night"
)
orig_levels <- levels(arrival_circadian_fac)
year_fac <- as.factor(data_env$year)
orig_year_levels <- levels(year_fac)
tag_serial_number_fac <- as.factor(data_env$tag_serial_number)
orig_tag_levels <- levels(tag_serial_number_fac)

data_env_t <- data_env %>%
  select(
    label_bin,
    Tw,
    Q,
    V,
    photoperiod,
    R,
    tag_serial_number,
    year,
    arrival_circadian
  ) %>%
  mutate(
    label_bin = factor(
      label_bin,
      levels = c(0, 1),
      labels = c("resident", "migration")
    ),
    tag_serial_number = as.numeric(tag_serial_number),
    year = as.numeric(year),
    arrival_circadian = as.numeric(arrival_circadian_fac)
  )
data_balanced <- themis::smote(data_env_t, "label_bin", over_ratio = 1) %>%
  mutate(
    tag_serial_number = factor(
      orig_tag_levels[tag_serial_number],
      levels = orig_tag_levels
    ),
    year = factor(
      orig_year_levels[as.numeric(year)],
      levels = orig_year_levels
    ),
    arrival_circadian = as.character(factor(
      orig_levels[arrival_circadian],
      levels = orig_levels
    )),
    label_bin = as.numeric(recode(label_bin, "resident" = 0, "migration" = 1))
  )

#correlogram balanced
pairs(
  data_balanced %>% dplyr::select(Tw, Q, V, photoperiod, R),
  col = adjustcolor(col_vec, alpha.f = 0.5),
  pch = 22
)

#correlation matrix
cor_mat <- data_balanced %>%
  dplyr::select(Tw, Q, V, photoperiod, R) %>%
  cor(use = "pairwise.complete.obs")

######################################################
# Build model

#glm
first_mod_glm <- glm(
  label_bin ~ Tw_an + Q_an + R + photoperiod,
  data = data_env,
  family = binomial(link = "cloglog")
)
summary(first_mod_glm)

#glmm
mc_size <- 10^5
clust <- makeCluster(4) #clusterise to decrease runtime
mixed <- "tag_serial_number" #list("tag_serial_number", "year")

mod_tag <- glmm(
  label_bin ~ Tw + Q + V + arrival_circadian + R,
  ~ 0 + tag_serial_number,
  varcomps.names = mixed,
  data = data_balanced,
  family = bernoulli.glmm,
  m = mc_size,
  cluster = clust
)

mod_tag <- glmer(
  label_bin ~ Q_an + R + Tw_an + photoperiod + (1 | tag_serial_number),
  data = data_env,
  family = binomial(link = "cloglog"),
  control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 500,
  #contrasts = list(departure_circadian = "contr.sum")
)
summary(mod_tag)
simRes <- simulateResiduals(fittedModel = mod_tag, n = 10000) # n as desired
plot(simRes)
time_num <- as.numeric(data_env$arrival) # or data_env$time (numeric index)
DHARMa::testTemporalAutocorrelation(simRes, time = time_num, plot = TRUE)
#plot in function of arrival
plot()
######################################################
#WITH AUTOCORRELATION
data_env <- data_env %>%
  arrange(tag_serial_number, arrival) %>%
  group_by(tag_serial_number) %>%
  mutate(
    time_numeric = as.numeric(
      arrival %--% lag(departure),
      # optional: integer index (e.g. 15-min bins) if you prefer discrete occasions
      time_index = as.integer(round(time_numeric / (15 * 60))),
      timef = factor(time_index)
    )
  ) %>%
  ungroup() %>%
  mutate(tag_serial_number = factor(tag_serial_number))


glmmTMB_mod <- glmmTMB(
  label_bin ~ Q_an +
    Tw_an +
    photoperiod +
    R +
    ar1(time + 0 | tag_serial_number),
  data = data_env,
  family = binomial(link = "cloglog"),
  control = glmmTMBControl(optimizer = "nlminb", optCtrl = list(iter.max = 1e5))
)
summary(glmmTMB_mod)


######################################################
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

g <- plot(first_mod$fitted.values, first_mod$residuals) +
  abline(h = 0, col = "red") +
  lines(
    loess.smooth(first_mod$fitted.values, first_mod$residuals),
    col = "blue",
    lwd = 2
  )

#monte carlo size large enough?
#checking that the MC se are way smaller than the se of the coeffiecient estimates
#se(mod_tag) vs mcse(mod_tag)
#10^3 NOT LARGE ENOUGH

#################################################################################
#Check performance
mod <- mod_tag

#accuracy
# Get predicted probabilities
pred_probs <- predict(mod_tag, type = "response")

# Convert probabilities to class labels (threshold 0.5)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# True labels (make sure to use the same data as in the model)
true_class <- data_env$label_bin #data_balanced$label_bin

# Calculate accuracy
accuracy <- mean(pred_class == true_class)
print(accuracy)


##################################################################################
#Check random effects variance (ICC)
vc <- as.data.frame(VarCorr(mod_tag))
# replace "tag_serial_number" with your grouping factor name
group_var <- vc$vcov[vc$grp == "tag_serial_number"]
latent_resid_var <- pi^2 / 3 #costanct value: Nakagawa & Schielzeth (2013, 2017)
icc_latent <- group_var / (group_var + latent_resid_var)


##################################################################################
#Discussion

# weird values for Tw and Q?
#Alles op een hoop --> meer migratory datapoints bij lage Tw
#MAAR houd bv. Q constant --> bij stijging in Tw minder
# OPMERKELIJK!
# HET COMBINEREN VAN VARIABELEN GEEFT EEN OMGEKEERDE RELATIE MET T_W
# HOGERE WAARDEN VAN T_W LEIDEN TOT EEN HOGERE KANS OP MIGRATIE
# Zet Q vast --> verhoog Tw --> migratiekans stijgt

#plot Tw and circadian oncircular plot
Time_min <- min(data$arrival)
Time_max <- max(data$departure)
All_arrival_days <- unique(round(data$arrival, units = "days"))
L10_077_Tw <- read_csv(
  './data/interim/processed/L10_077_Tw.csv',
  show_col_types = FALSE
) %>%
  filter(round(Timestamp, units = "days") %in% All_arrival_days) %>%
  mutate(
    hour_arrival = factor(hour(Timestamp), levels = as.character(0:23)),
    day_arrival = factor(round(Timestamp, units = "days"))
  ) %>%
  group_by(day_arrival) %>%
  mutate(T_scale = Value - mean(Value, na.rm = TRUE)) %>%
  ungroup()


data <- data %>%
  mutate(hour_arrival = factor(hour(arrival), levels = as.character(0:23))) %>%
  filter(!is.na(cluster))

p <- ggplot() +
  geom_bar(data = data, aes(fill = arrival_circadian, x = hour_arrival)) + #aes(fill = arrival_circadian)
  geom_boxplot(
    data = L10_077_Tw,
    aes(y = T_scale * 9 + 20, x = hour_arrival),
    fill = NA,
    color = "#544545",
    width = 0.7
  ) +
  coord_radial(r.axis.inside = TRUE, expand = FALSE, direction = 1) +
  theme(
    #axis.title.y = element_text(size = 30),
    axis.text.r = element_text(size = 27, color = "black"),
    #axis.text.y = element_text(size = 27, color = "black"),
    axis.text.y.right = element_text(size = 27, color = "#544545")
  ) +
  style +
  guides(
    fill = guide_legend(title = "Circadian phase"),
    r.sec = guide_axis(
      theme = theme(axis.text.r = element_text(colour = "#544545"))
    )
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ (. - 20) / 9,
      name = "Tw (°C)"
    ),
    name = "# eels"
  ) +
  scale_x_discrete(drop = FALSE)
plot(p)
ggsave(
  "./figures/Circadian/circadian_non_tidal_TW.png",
  width = 10,
  height = 10
)

######################################################
# GLMM Trigger
# How are the environm. conditions different in de starting period of migration VS before?
######################################################
# with interpolated data
range <- as.period(1, "days") #1 day range
data$trigger <- NA
data_mod <- data %>%
  group_by(tag_serial_number) %>%
  filter(row_number() <= first_migratory_idx[1]) %>%
  mutate(
    trigger = ifelse(date > (date[length(date)] - as.duration(range)), 1, 0),
    tag_serial_number = as.factor(tag_serial_number),
    Tw = scale(Tw, scale = true_scale),
    delta_Tw = scale(delta_Tw, scale = true_scale),
    photoperiod = scale(photoperiod, scale = true_scale),
    Q = scale(Q, scale = true_scale),
    delta_Q = scale(delta_Q, scale = true_scale),
    V = scale(V, scale = true_scale),
    R = scale(R, scale = true_scale) #1 part of trigger, 0 rest
  ) %>%
  ungroup()

L10_077_Tw <- read_csv(
  './data/interim/processed/L10_077_Tw.csv',
  show_col_types = FALSE
) %>%
  dplyr::select(Timestamp, Value) %>%
  rename(Tw = Value) %>%
  mutate(Tw = scale(Tw, scale = true_scale))

L10_077_Q <- read_csv(
  './data/interim/processed/L10_077_Q.csv',
  show_col_types = FALSE
) %>%
  dplyr::select(Timestamp, Value) %>%
  rename(Q = Value) %>%
  mutate(Q = scale(Q, scale = true_scale))

L10_077_V <- read_csv(
  './data/interim/processed/L10_077_V.csv',
  show_col_types = FALSE
) %>%
  dplyr::select(Timestamp, Value) %>%
  rename(V = Value) %>%
  mutate(V = scale(V, scale = true_scale))

data_plot <- data_mod %>%
  left_join(L10_077_Tw, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_Q, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_V, by = c("date" = "Timestamp"), copy = TRUE)

#glm
first_mod_glm <- glm(
  trigger ~ Tw.y + V.y + R + Q.y,
  data = data_plot,
  family = binomial
)
summary(first_mod_glm)

mod_tag <- glmer(
  trigger ~ (1 | tag_serial_number) + Q + delta_Q + delta_Tw, #arrival_circadian
  data = data_mod,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  #nAGQ = 10,
  contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)


# with raw data
# resident Vs first migratory point
data_env <- data_env %>%
  group_by(tag_serial_number) %>%
  filter(row_number() <= first_migratory_idx[1]) %>%
  ungroup() %>%
  mutate(date = round_date(arrival, unit = "15M 0S"))

data_plot <- data_env %>%
  left_join(L10_077_Tw, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_Q, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_V, by = c("date" = "Timestamp"), copy = TRUE)

mod_tag <- glmer(
  label_bin ~ (1 | tag_serial_number) + Q.y, # + R + Tw.y,# + Tw.y + R + arrival_circadian,
  data = data_plot,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 10,
  contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)

ggplot(data_env) +
  geom_point(aes(x = V, y = photoperiod))

######################################################################
