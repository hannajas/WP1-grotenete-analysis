library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyquant)
library(patchwork)
library(geosphere)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/inverse_distance_function.R")
source("./src/concat_all_env_var_functions.R")
source("./src/align_resolutions_function.R")

# Upload dataset
data <- read_csv('./data/interim/migration.csv', show_col_types = FALSE)
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# META-DATA
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

# averaging the environmental variables to fit telemetry data
variables <- c("Tw", "Q", "photoperiod", "R", "S", "turb", "O", "V")
p <- 1
for (var in variables) {
  temp <- concat_all_env_vars(data, var, metadata)
  assign(paste0("env_data_", var), temp)
  if (var == "photoperiod") {
    data[[var]] <- env_data_photoperiod$photoperiod
    next
  }
  if (var == "Q") {
    env_data_Q_norm <- env_data_Q / colMeans(env_data_Q, na.rm = TRUE)
    data$Q <- inverse_distance(data, env_data_Q_norm, metadata, p, "Q")
    next
  }
  data[[var]] <-
    inverse_distance(data, get(paste0("env_data_", var)), metadata, p, var)
}
# env_data_Tw <- concat_all_env_vars(data, "Tw", metadata)
# env_data_Q <- concat_all_env_vars(data, "Q", metadata)
# env_data_photoperiod <- concat_all_env_vars(data, "photoperiod", metadata)
# env_data_R <- concat_all_env_vars(data, "R", metadata)
# env_data_S <- concat_all_env_vars(data, "S", metadata)
# env_data_turb <- concat_all_env_vars(data, "turb", metadata)
# env_data_O <- concat_all_env_vars(data, "O", metadata)
# env_data_V <- concat_all_env_vars(data, "V", metadata)

# #divide dataframe env_data_Q by its column means (to correct for wide range of Q-values)
# env_data_Q_norm <- env_data_Q / colMeans(env_data_Q, na.rm = TRUE)

# # INVERSE DISTANCE WEIGHTING
# data$Tw <- inverse_distance(data, env_data_Tw, metadata, p, "Tw") # in deze functie nog filteren in meta data
# data$Q <- inverse_distance(data, env_data_Q_norm, metadata, p, "Q")
# data$photoperiod <- env_data_photoperiod$photoperiod
# data$R <- inverse_distance(data, env_data_R, metadata, p, "R")
# data$S <- inverse_distance(data, env_data_S, metadata, p, "S")
# data$turb <- inverse_distance(data, env_data_turb, metadata, p, "turb")
# data$O <- inverse_distance(data, env_data_O, metadata, p, "O")
# data$V <- inverse_distance(data, env_data_V, metadata, p, "V")

#CALCULATE THE DELTA VALUES
data_list <- split(data, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- c(NaN, diff(x$Tw))
  x$delta_Q <- c(NaN, diff(x$Q))
  return(x)
})
data <- plyr::ldply(data_temp, data.frame)

# filter out unrealistic speed values
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

#write.csv(data_filter, './data/interim/migration_env_filter.csv')

#######################################
# Link environmental data with INTERPOLATED telemetry data
#######################################
p <- 1
source("./src/inverse_distance_function.R")
source("./src/align_resolutions_function.R")
data_inter_env <- read_csv(
  './data/interim/migration_inter.csv',
  show_col_types = FALSE
)
variables <- c("Tw", "Q", "V", "O", "turb", "S") #,"R")
for (var in variables) {
  if (!var %in% colnames(data_inter_env)) {
    stop(paste("Variable", var, "not found in data_inter_env."))
  }
  temp <- align_resolutions_function(
    var,
    as.difftime(5, units = "mins"), #as.period(5, "mins"),
    metadata,
    upsample_method = "ffill",
    data_inter_env
  )
  assign(paste0("env_data_", var), temp)
  data_inter_env[[var]] <-
    inverse_distance(
      data_inter_env,
      get(paste0("env_data_", var)),
      metadata,
      p,
      var
    )
}
#set photoperiod right
photoperiod <- read_csv(
  "./data/interim/processed/photoperiod_photoperiod.csv"
) %>%
  select(Value, date) %>%
  mutate(date = as.POSIXct(date, tz = "UTC")) %>%
  rename(photoperiod = Value)
data_inter_env$rounddate <- floor_date(data_inter_env$date, "day")
data_inter_env <- left_join(
  data_inter_env,
  photoperiod,
  by = c("rounddate" = "date")
) %>%
  select(-rounddate)


data_list <- split(data_inter_env, data_inter_env$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- x$Tw - lag(x$Tw)
  x$delta_Q <- x$Q - lag(x$Q)
  return(x)
})
data_inter_env <- plyr::ldply(data_temp, data.frame)

write_csv(data_inter_env, "./data/interim/migration_env_inter.csv")
