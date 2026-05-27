library(geosphere)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(activity)

########################################################
#make selection in the data
########################################################
true_scale <- TRUE

# load and process data
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#INTERPOLATED DATA
# data_raw <- read.csv(
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

#RAW DATA
data_raw <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
) %>%
  group_by(tag_serial_number)

#als dat a temp een kolom data bevat
no_detections <- c(1171747, 1171751, 1294168, 1294172) #leave out eels with only one detection
no_onset <- c(
  #leave out the eels without onset
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
  filter(zone == "non-tidal") %>% #| zone == "transition") %>%#only non-tidal
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
    #fill 'resident' and 'migration'
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
var <- "speed_m_s"
data_env %>%
  mutate(var = .data[[var]]) %>%
  dplyr::select(var, label) %>%
  group_by(label) %>%
  summarise(
    mean_R = mean(var, na.rm = TRUE),
    median_R = median(var, na.rm = TRUE)
  )

variables <- c(
  #"speed_m_s",
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

p <- ggplot(data_env_long, aes(x = label, y = value, fill = label)) + #choose the colors
  scale_fill_manual(values = c("resident" = grey1, "migration" = "white")) +
  geom_boxplot(position = "dodge") + #add jitter
  # geom_jitter(
  #   position = position_jitter(width = 0.2, height = 0),
  #   alpha = 0.2,
  #   size = 2,
  #   contour.color = "black",
  # ) +
  facet_wrap(
    ~variable,
    nrow = 1,
    scales = "free",
    labeller = as_labeller(
      c(
        #"speed_m_s" = "v~\"(m/s)\"",
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
    strip.text = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  windows(width = 16, height = 6) #was 5
plot(p)
# ggsave(
#   "./figures/Clustering/boxplot_labelled_non_tidal_Qscaled.png"
# )

######################################################
# Short-term trigger - INTERPOLATED DATA
######################################################
#USE INTERPOLATE DATA (uncommnent line 16-27)
variable <- "Q" #Tw?R?
delta_variable <- "delta_Q" #delta_Tw? delta_R?

list <- split(data_env, data_env$tag_serial_number) # list of all eels
# identify the first breakpoint (time point t_k) at location x_k (migratory for the first time)
#empty dataframe
datatemp <- data.frame()
# identify range [t_k - r; t_k]
range <- as.period(1, "days")

for (i in 1:length(list)) {
  data_eel <- list[[i]] # select one eel
  # identify the first breakpoint (time point t_k) at location x_k (migratory for the first time)
  #t_k <- data_eel$date[data_eel$first_migratory_idx[1]]
  t_k <- data_eel$date[which(data_eel$label == "migration")[1]]
  if (is.na(t_k)) {
    next
  }
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
  #merge with datatemp
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
    color = "grey1",
    linetype = "dashed"
  ) +
  facet_wrap(~tag_serial_number) +
  style + #other text on y axis, no x values in x  axis
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank()) +
  labs(y = "Discharge (m³/s)", x = "Eel ID")
plot(g)
ggsave(
  "./figures/onset_of_migration/as_trigger/Q_rangemmax_1day_delday1.png",
  plot = g,
  width = 14,
  height = 16
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
# Time of onset (WITH RAW DATA)
######################################################
# data_onset <- data %>%
#   group_by(tag_serial_number) %>%
#   filter(row_number() == first_migratory_idx[1])

p <- ggplot(data_onset, aes(x = hour(departure))) + #hour(departure)
  geom_bar(fill = grey1) + #"#F39C12"
  coord_radial(r.axis.inside = TRUE, expand = FALSE) + # rotate so 0 is at north (start = -pi/120, direction = 1)
  scale_x_continuous(
    breaks = seq(0, 21, by = 3), # 0,3,6,9,12,15,18,21
    limits = c(0, 24) # ensure full circle
  ) +
  # scale_x_continuous(
  #   breaks = seq(0, 2 * pi, by = pi / 4),
  #   labels = parse(text = c("0", "pi/4", "pi/2", "3*pi/4", "pi","5*pi/4", "3*pi/2", "7*pi/4", "2*pi")),
  #   limits = c(0, 2 * pi) # ensure full circle
  # ) +
  style +
  theme(
    #axis.line = element_blank(),
    strip.text = element_text(size = 25),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)
  ) +
  labs(
    #title = "Onset of migration",
    x = "Hour of onset",
    y = element_blank()
  ) + #change position of label y axis
  # guides(fill = guide_legend(title = "Circadian phase")) +
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
print(p)
#save plot
ggsave(
  "./figures/onset_of_migration/onset_migration_bw.png",
  width = 7,
  height = 7
)


############################################################################################################
# GLMM Conditions
# How are the environmental conditions different from an eel that started migration VS an eel still resident
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
# Build model
#glmm
mod_tag <- glmer(
  label_bin ~ Q_an + photoperiod + (1 | tag_serial_number) + R,
  data = data_env,
  family = binomial(link = "cloglog"),
  control = glmerControl(optimizer = "bobyqa"),
  nAGQ = 90
)
summary(mod_tag)

######################################################
#check assumptions

#Is singular?
isSingular(mod_tag, tol = 1e-4) #FALSE =OK

#iNDEPENDENCE OF OBSERVATIONS (conditional on random effects)
#Use simulated residuals to check assumptions

sim <- simulateResiduals(fittedModel = mod_tag, n = 10000)
plot(sim) # overall diagnostics
testUniformity(sim) # residual distribution
testDispersion(sim) # over/underdispersion
testZeroInflation(sim)
testOutliers(sim) #no outliers

# Test autocorrelation
time_num <- as.numeric(data_env$arrival)
DHARMa::testTemporalAutocorrelation(
  sim,
  time = as.numeric(data_env$arrival),
  plot = TRUE
)
# OK

#RANDOM EFFECTS NORMALITY CHECK
ranef_mod <- ranef(mod_tag, condVar = TRUE)
qqnorm(ranef_mod$tag_serial_number[, 1], main = "Q-Q Plot of Random Effects")
qqline(ranef_mod$tag_serial_number[, 1], col = "red") #ok


# Check collinearity along fixed effects
library(performance)
check_collinearity(mod_tag)
#OK

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
icc_latent
