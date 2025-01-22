# Function to to link one environmental variable to the processed telemetry data
concat_env_var <- function(telemetry_data, env_data, resolution, name) {
    #round the departure and arrival so that it fits the environmental resolution
    dep_time <- round_date(telemetry_data$departure,unit = resolution)
    arr_time <- round_date(telemetry_data$arrival,unit = resolution)
    telemetry_data$name <- lapply(1:length(dep_time), function(i,x) {
    mean(x$Value[x$Timestamp >= arr_time[i] & x$Timestamp <= dep_time[i]], na.rm=TRUE)    
    }, x = L07_077_Tw)
    return(telemetry_data)
}
