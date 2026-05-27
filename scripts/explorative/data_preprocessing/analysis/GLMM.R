######################################################
# ONSET OF MIGRATION
######################################################
######################################################
library(themis) #package to deal with unbalanced data
# RANDOM UNDERSAMPLING
#from 02_onset_migration.R
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


table(data_balanced$label)
##########################################################################
#GLM
first_mod_glm <- glm(
  label_bin ~ Tw_an + Q_an + R + photoperiod,
  data = data_env,
  family = binomial(link = "cloglog")
)
summary(first_mod_glm)

##########################################################################
#GLMM
mc_size <- 10^5
#monte carlo size large enough?
#checking that the MC se are way smaller than the se of the coeffiecient estimates
#se(mod_tag) vs mcse(mod_tag)
#10^3 NOT LARGE ENOUGH
clust <- makeCluster(4) #clusterise to decrease runtime
mixed <- "tag_serial_number" #list("tag_serial_number", "year")

mod_tag <- glmm(
  label_bin ~ Tw_an + Q_an + R + photoperiod,
  ~ 0 + tag_serial_number,
  varcomps.names = mixed,
  data = data_env,
  family = binomial(link = "cloglog"),
  m = mc_size,
  cluster = clust
)
###########################################################################
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


##################################################################################
#plot Tw and circadian on circular plot
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

######################################################
# DURING MIGRATION
######################################################
first_mod_glm <- glm(
  log(speed_m_s) ~ Q + photoperiod + R + Tw, #delta_Tw + delta_Q + R + Tw + photoperiod + V
  data = data_env
)

first_mod_glm <- glm(
  label_bin ~ delta_Q + photoperiod, #delta_Tw + delta_Q + R + Tw + photoperiod + V
  data = data_env,
  family = binomial
)
summary(first_mod_glm)
