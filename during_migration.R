library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)

########################################################
#make selection in the data
########################################################
#non-tidal?,
#delete day resident phase (keep resting phase)
#leave out eels with only one detection (1171747, 1171751, 1294168, 1294172)

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
#   group_by(tag_serial_number)

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
        "resident", #resident
      label == "resident/resting" &
        row_number() > first_migratory_idx ~
        "resting",
      label == "migratory" ~ "migratory",
      label == is.infinite(first_migratory_idx) | is.na(first_migratory_idx) ~
        "resident"
    )
  ) %>%
 #filter(date >= date[first_migratory_idx] - days(1)) %>%
 #filter(label != "resident") %>%
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

########################################################
# GLMM Conditions during migration
########################################################
# migratory is there the "default" so migratory = 0
########################################################
#Prepare data
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
      labels = c("migratory", "resting")
    ),
    tag_serial_number = as.factor(tag_serial_number),
    Tw = scale(Tw, scale = true_scale),
    photoperiod = scale(photoperiod, scale = true_scale),
    Q = scale(Q, scale = true_scale),
    deltaQ = scale(delta_Q, scale = true_scale),
    delta_Tw = scale(delta_Tw, scale = true_scale),
    delta_R = scale(delta_R, scale = true_scale),
    V = scale(V, scale = true_scale),
    R = scale(R, scale = true_scale) #1 part of trigger, 0 rest
  ) %>%
  filter(!is.na(label_bin)) %>%
  ungroup()


######################################################
#Analyse the data
#Is the data skewed?
skew_table <- table(data_env$label_bin)

cols <- as.factor(data_env$label_bin)
palette <- c("resting" = "red", "migratory" = "blue")
col_vec <- palette[as.character(cols)]
pairs(
  data_env %>% dplyr::select(Tw, Q, V, photoperiod, R),
  col = adjustcolor(col_vec, alpha.f = 0.5),
  pch = 22
)

cor_mat <- data_env %>%
  dplyr::select(Tw, Q, V, photoperiod, R) %>%
  cor(use = "pairwise.complete.obs")
#delta_Tw and delta_Q

ggplot(data_env, aes(x = delta_Q, y = delta_Tw, color = label_bin)) +
  geom_point()

######################################################
# Built model
mod_tag <- glmer(
  label_bin ~ (1 | tag_serial_number) + Q + R + photoperiod + Tw, #arrival_circadian + photoperiod + Q + Tw
  data = data_env,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  #nAGQ = 10,
  contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)


mod_tag <- glmer(
  speed_m_s ~ (1 | tag_serial_number) + Q + R + photoperiod + Tw, #arrival_circadian + photoperiod + Q + Tw
  data = data_env,
  family = gaussian,
  #control = glmerControl(optimizer = "bobyqa"),
  #nAGQ = 10,
  contrasts = list(arrival_circadian = "contr.sum")
)
summary(mod_tag)

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

pred_probs <- predict(mod_tag, type = "response")

# Convert probabilities to class labels (threshold 0.5)
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# True labels (make sure to use the same data as in the model) factor to 0 and 1
true_class <- as.numeric(data_env$label_bin) - 1

# Calculate accuracy
accuracy <- mean(pred_class == true_class)
print(accuracy)

######################################################
# Visualize for each eel seperately
data_plot <- data_env %>%
  filter(tag_serial_number == 1305785)
plot_1 <- ggplot(data_plot) +
  geom_point(aes(x = date, y = V, color = label_bin), size = 2) + #color scale label_bin = 1 --> red
  geom_point(aes(x = date, y = Tw, color = label_bin), size = 2, shape = "+") +
  scale_color_manual(
    values = c("resting" = "red", "migratory" = "blue"),
    name = "State"
  ) +
  facet_wrap(~tag_serial_number, scales = "free")
plot_1

# plot L07 Q en Tw
L10_077_Tw <- read_csv(
  './data/interim/processed/L07_077_Tw.csv',
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

data_plot <- data_env %>%
  left_join(L10_077_Tw, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_Q, by = c("date" = "Timestamp"), copy = TRUE) %>%
  left_join(L10_077_V, by = c("date" = "Timestamp"), copy = TRUE)
plot_1 <- ggplot(data_plot) +
  geom_point(aes(x = date, y = Q.y, color = label), size = 2) + #color scale label_bin = 1 --> red
  geom_point(
    aes(x = date, y = Tw.y, color = label),
    size = 1,
    shape = "+"
  ) +
  scale_color_manual(
    values = c("resting" = "red", "migratory" = "blue", "resident" = "green"),
    name = "State"
  ) +
  facet_wrap(~tag_serial_number, scales = "free")
plot_1
