# Function to to link one environmental variable to the processed telemetry data
concat_env_var <- function(telemetry_data, metadata) {
    #round the departure and arrival so that it fits the environmental resolution
    path <- paste('./data/interim/processed/',metadata$name[1],'_Tw.csv', sep ="")
    env_data <- read_csv(path)
    env_data$Timestamp <- ymd_hms(env_data$Timestamp)
    dep_time <- round_date(telemetry_data$departure,unit = metadata$resolution[1])
    arr_time <- round_date(telemetry_data$arrival,unit = metadata$resolution[1])

    # calculate the mean temperature between arrival an departure
    telemetry_data$temp <- unlist(lapply(1:length(dep_time), function(i,x) { 
        mean(x$Value[x$Timestamp >= arr_time[i] & x$Timestamp <= dep_time[i]], na.rm=TRUE) }, x = env_data))
    return(telemetry_data$temp)
}
