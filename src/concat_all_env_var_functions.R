# function to interpolate all environmental variables
concat_all_env_vars <- function(telemetry_data, data_type, metadata) {
    metadata_filter <- filter(metadata, metadata$type == data_type)
    #print(paste(metadata_filter))
    n <- dim(metadata_filter)[1]
    for (i in 1:n) {
        path <- paste('./data/interim/processed/',metadata_filter$name[i],'_',data_type,'.csv', sep ="")
        #print(paste(path))
        env_data <- read_csv(path, show_col_types = FALSE)
        env_data$Timestamp <- ymd_hms(env_data$Timestamp,truncated = 3)#ydm for temperature

        telemetry_data[metadata_filter$name[i]] <- concat_env_var(telemetry_data, metadata_filter[i,], env_data)
    }

    env_data_temp <- telemetry_data[(dim(telemetry_data)[2]-(n-1)):dim(telemetry_data)[2]]
    return(env_data_temp)
}