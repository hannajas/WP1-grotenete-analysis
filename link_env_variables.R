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
data <- read_csv('./data/interim/migration_filter.csv', show_col_types = FALSE)
data$...1 <- NULL
data$arrival <- ymd_hms(data$arrival)
data$departure <- ymd_hms(data$departure)

# META-DATA
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

#Upload lookup
lookup <- read_csv(
  './data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv',
  show_col_types = FALSE
)

# distance to splitsing (rupel --> schelde)
dist_split <- lookup %>%
  filter(NAAM == "rup") %>%
  dplyr::select(distance) %>%
  max() #Rupel --> Zeescheldt
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
  # if (var == "R") {
  #   next
  # }
  data[[var]] <-
    inverse_distance(data, get(paste0("env_data_", var)), metadata, p, var)
}
#env_data_R <- concat_all_env_vars(data, "R", metadata)
#data$Tw <- inverse_distance(data, env_data_Tw, metadata, p, "Tw") # in deze functie nog filteren in meta data

# set accumulated R right --> accumulation from release date onwards
# measurements <- names(env_data_R)
# env_data_R$tag_serial_number <- data$tag_serial_number
# env_data_accR <- env_data_R %>%
#   group_by(tag_serial_number) %>%
#   mutate(across(all_of(measurements), cumsum)) %>%
#   ungroup() %>%
#   dplyr::select(-tag_serial_number)
# data$R <-
#   inverse_distance(data, env_data_accR, metadata, p, "R")

#CALCULATE THE DELTA VALUES
data_list <- split(data, f = data$tag_serial_number)
data_temp <- lapply(data_list, function(x) {
  x$delta_Tw <- c(NaN, diff(x$Tw))
  x$delta_Q <- c(NaN, diff(x$Q))
  x$delta_R <- c(NaN, diff(x$R))
  return(x)
})
data <- plyr::ldply(data_temp, data.frame)

#set NA all S values where zone non-tidal
data$S[data$zone == "non-tidal"] <- NA
data$turb[data$zone == "non-tidal"] <- NA
data$O[data$zone == "non-tidal"] <- NA

write.csv(data, './data/interim/migration_env_filter.csv')
