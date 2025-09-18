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

##############################################################################
# Link environmental data with RAW telemetry data
##############################################################################
variables <- c("Tw", "Q", "photoperiod", "R", "S", "turb", "O", "V")
p <- 1
for (var in variables) {
  temp <- concat_all_env_vars(data, var, metadata) # averaging the environmental variables to fit telemetry data
  assign(paste0("env_data_", var), temp)
  if (var == "photoperiod") {
    data[[var]] <- env_data_photoperiod$photoperiod
    next
  }
  if (var == "Q") {
    env_data_Q_norm <- env_data_Q #/ colMeans(env_data_Q, na.rm = TRUE)
    data$Q <- inverse_distance(data, env_data_Q_norm, metadata, p, "Q") # INVERSE DISTANCE WEIGHTING
    next
  }
  if (var == "R") {
    next
  }
  data[[var]] <-
    inverse_distance(data, get(paste0("env_data_", var)), metadata, p, var)
}
#env_data_R <- concat_all_env_vars(data, "R", metadata)
# data$Tw <- inverse_distance(data, env_data_Tw, metadata, p, "Tw") # in deze functie nog filteren in meta data

# set accumulated R right --> accumulation from release date onwards
measurements <- names(env_data_R)
env_data_R$tag_serial_number <- data$tag_serial_number
env_data_accR <- env_data_R %>%
  group_by(tag_serial_number) %>%
  mutate(across(all_of(measurements), cumsum)) %>%
  ungroup() %>%
  select(-tag_serial_number)
data$R <-
  inverse_distance(data, env_data_accR, metadata, p, "R")

#CALCULATE THE DELTA VALUES
data_list <- split(data, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- c(NaN, diff(x$Tw))
  x$delta_Q <- c(NaN, diff(x$Q))
  x$delta_R <- c(NaN, diff(x$R))
  return(x)
})
data <- plyr::ldply(data_temp, data.frame)

# filter out unrealistic speed values
data_filter <- filter(data, !startsWith(data$station_name, "ws-")) #+- 62 waarden uitgelaten

write.csv(data_filter, './data/interim/migration_env_filter.csv')

##############################################################################
# Link environmental data with INTERPOLATED telemetry data
##############################################################################
p <- 1
source("./src/inverse_distance_function.R")
source("./src/align_resolutions_function.R")
#data from smooting has a resolution of 15 min
data_inter_env <- read_csv(
  './data/interim/migration_inter.csv',
  show_col_types = FALSE
) %>%
  select(-photoperiod)
variables <- c("Tw", "Q", "V", "O", "turb", "S", "R")
for (var in variables) {
  #runt lang!
  if (!var %in% colnames(data_inter_env)) {
    stop(paste("Variable", var, "not found in data_inter_env."))
  }
  # if (var == "O" | var == "turb" | var == "S") {
  #   temp <- align_resolutions_function(
  #     #R --> all data on 15 min
  #     var,
  #     as.difftime(5, units = "mins"), #as.period(5, "mins"),
  #     metadata,
  #     upsample_method = "ffill",
  #     data_inter_env
  #   )
  # } else {
  temp <- align_resolutions_function(
    #R --> all data on 15 min
    var,
    as.difftime(15, units = "mins"), #as.period(5, "mins"),
    metadata,
    upsample_method = "ffill",
    data_inter_env
  )
  # }

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
  dplyr::select(Value, date) %>%
  mutate(date = as.POSIXct(date, tz = "UTC")) %>%
  rename(photoperiod = Value)
data_inter_env$rounddate <- floor_date(data_inter_env$date, "day")
data_inter_env <- left_join(
  data_inter_env,
  photoperiod,
  by = c("rounddate" = "date")
) %>%
  dplyr::select(-rounddate)

#set R right
# left_join(data_inter_env, env_data_R, by = "date")
# temp <- align_resolutions_function(#R --> all data on 15 min
#   "R",
#   as.difftime(15, units = "mins"), #as.period(5, "mins"),
#   metadata,
#   upsample_method = "ffill",
#   data_inter_env
# )
# temp_7 <-
#   inverse_distance(
#     data_inter_env,
#     temp,
#     metadata,
#     p,
#     "R"
#   )
data_inter_env <- data_inter_env %>%
  group_by(tag_serial_number) %>%
  mutate(R = cumsum(R)) %>%
  ungroup()


data_list <- split(data_inter_env, data_inter_env$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- x$Tw - lag(x$Tw)
  x$delta_Q <- x$Q - lag(x$Q)
  x$delta_R <- x$R - lag(x$R)
  return(x)
})
data_inter_env <- plyr::ldply(data_temp, data.frame)

write_csv(data_inter_env, "./data/interim/migration_env_inter.csv")
