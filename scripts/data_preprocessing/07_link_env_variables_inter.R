library(tidyquant)
library(patchwork)
library(geosphere)

# Source functions
source("./src/concat_env_var_function.R")
source("./src/inverse_distance_function.R")
source("./src/concat_all_env_var_functions.R")
source("./src/align_resolutions_function.R")

# Upload dataset
data <- read_csv('./data/interim/migration_filter.csv', show_col_types = FALSE)
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# META-DATA
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

##############################################################################
# Link environmental data with INTERPOLATED telemetry data
##############################################################################
p <- 1
#data from smooting has a resolution of 15 min
data_inter_env <- read_csv(
  paste("./data/interim/migration_inter_", resolution_s, ".csv", sep = ""),
  show_col_types = FALSE
) %>%
  dplyr::select(-photoperiod)
variables <- c("Tw", "Tw_an", "Q_an", "Q", "V", "O", "turb", "S", "R")
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
#   "S",
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

#cumulate R
# data_inter_env <- data_inter_env %>%
#   group_by(tag_serial_number) %>%
#   mutate(R = cumsum(R)) %>%
#   ungroup()

data_list <- split(data_inter_env, data_inter_env$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- x$Tw - lag(x$Tw)
  x$delta_Q <- x$Q - lag(x$Q)
  #x$delta_R <- x$R - lag(x$R)
  return(x)
})
data_inter_env <- plyr::ldply(data_temp, data.frame)

# set NA all S values where zone non-tidal
data_inter_env$S[data_inter_env$zone == "non-tidal"] <- NA
data_inter_env$turb[data_inter_env$zone == "non-tidal"] <- NA
data_inter_env$O[data_inter_env$zone == "non-tidal"] <- NA

write_csv(data_inter_env, "./data/interim/migration_env_inter.csv")
