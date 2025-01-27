mean_env_var <- function(telemetry_data, metadata,p) {
    n <- nrow(metadata)

    for (i in seq_len(n)) {
        telemetry_data$temp <- concat_env_var(telemetry_data, metadata[i])
        names(telemetry_data)[dim(telemetry_data)[2]] <- metadata$name[i]
    }

    env_data <- telemetry_data[(dim(telemetry_data)[2]-(n-1)):dim(telemetry_data)[2]]
    V <- as.matrix(env_data)

    W <- lapply(1:n, function(i) {
        ifelse((telemetry_data$distance_to_source_m - metadata$distance_to_source[i]) ==0, NA, abs(1/(telemetry_data$distance_to_source_m - metadata$distance_to_source[i])^p))

    })
    W <- matrix(unlist(W), ncol = 2)
    telemetry_data$Tw <- rowSums(V*W)/rowSums(W)

    #unlist is important!!!
    #unlist(lapply(1:n, function(i) {
    #    data$Tw[data$station_name == metadata_Tw$receiver[i]] <- env_data[data$station_name == metadata_Tw$receiver[i],i]
    #    }))
    #Tw_old <- data$Tw
    #data$Tw[data$station_name == metadata_Tw$receiver[1]] <- env_data[data$station_name == metadata_Tw$receiver[1],1]
    for (i in 1:n) {
        telemetry_data$Tw <- replace(telemetry_data$Tw, telemetry_data$station_name == metadata$receiver[i],unlist(env_data[telemetry_data$station_name == metadata$receiver[i],i]))
    }
    #data$Tw <- replace(data$Tw, data$station_name == metadata_Tw$receiver[i],unlist(env_data[data$station_name == metadata_Tw$receiver[i],i]))

    #data$Tw[data$station_name == metadata_Tw$receiver[i]] <- env_data[data$station_name == metadata_Tw$receiver[i],i]
    return()
}