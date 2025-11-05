library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)

########################################################
#make selection in the data
########################################################

# load and process data
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#raw
data_env <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  group_by(tag_serial_number) #%>%


data_env <- data_env %>%
  filter(zone == "non-tidal") %>% #| zone == "transition") %>%
  filter(
    !tag_serial_number %in%
      c(1171747, 1171751, 1294168, 1294172)
  ) %>%
  #method PJ for onset migration
  mutate(
    migration = lag(migration),
    label = ifelse(
      migration == FALSE,
      "resident",
      ifelse(migration == TRUE, "migration", NA)
    )
  ) %>%
  mutate(
    label = case_when(
      label == "migration" &
        cluster == 1 ~
        "resident",
      label == "migration" &
        cluster == 2 ~
        "migratory",
      label == "resident" ~ "resident"
    )
  ) %>%
  ungroup()

data$label_bin <- NA
true_scale <- TRUE
data_env <- data %>%
  group_by(tag_serial_number) %>%
  #filter(row_number() >= first_migratory_idx[1]) %>%
  mutate(
    label_bin = replace(label_bin, cluster == 1, 1),
    label_bin = replace(label_bin, cluster == 2, 0), #migratory = 0 and resting = 1
    label_bin = factor(
      label_bin,
      levels = c(0, 1),
      labels = c("migratory", "resting"),
    ),
    tag_serial_number = as.factor(tag_serial_number),
    Tw_an = scale(Tw_an, scale = true_scale),
    Tw = scale(Tw, scale = true_scale),
    photoperiod = scale(photoperiod, scale = true_scale),
    Q_an = scale(Q_an, scale = true_scale),
    Q = scale(Q, scale = true_scale),
    deltaQ = scale(delta_Q, scale = true_scale),
    delta_Tw = scale(delta_Tw, scale = true_scale),
    delta_R = scale(delta_R, scale = true_scale),
    V = scale(V, scale = true_scale),
    R = scale(R, scale = true_scale) #1 part of trigger, 0 rest
  ) %>%
  filter(!is.na(label_bin)) %>%
  ungroup()


cor_mat <- data_env %>%
  dplyr::select(Tw, Q, V, photoperiod, R) %>%
  cor(use = "pairwise.complete.obs")

mod_tag <- glmer(
  label_bin ~ (1 | tag_serial_number) + photoperiod + Tw + R + Q,# arrival_circadian
  data = data_env,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  #nAGQ = 10,
  contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)

mod_tag <- glmer(
  log(speed_m_s) ~ (1 | tag_serial_number) + Q_an + R + Tw_an + photoperiod, #arrival_circadian + photoperiod + Q + Tw
  data = data_env,
  family = gaussian
  #control = glmerControl(optimizer = "bobyqa"),
  #nAGQ = 10,
  #contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)

first_mod_glm <- glm(
 speed_m_s ~ Q + photoperiod + R + Tw_an, #delta_Tw + delta_Q + R + Tw + photoperiod + V
  data = data_env
)
summary(first_mod_glm)
