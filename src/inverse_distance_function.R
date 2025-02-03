inverse_distance <- function(telemetry_data, env_data, metadata,p) {
#W <- matrix(, nrow = dim(data)[1], ncol = n)
n <- dim(metadata)[1]
V <- as.matrix(env_data)
nan_V <- which(is.nan(V))

W <- lapply(1:n, function(i) {
    ifelse((telemetry_data$distance_to_source_m - metadata$distance_to_source[i]) ==0, NA, abs(1/(telemetry_data$distance_to_source_m - metadata$distance_to_source[i])^p))

})

W <- matrix(unlist(W), ncol = n)
# dealing with the nan values in temperature values
W[nan_V] <-  0

telemetry_data$Tw <- rowSums(V*W, na.rm = TRUE)/rowSums(W, na.rm = TRUE)

#unlist is important!!!
#unlist(lapply(1:n, function(i) {
#    data$Tw[data$station_name == metadata_Tw$receiver[i]] <- env_data[data$station_name == metadata_Tw$receiver[i],i]
#    }))
#Tw_old <- data$Tw
#data$Tw[data$station_name == metadata_Tw$receiver[1]] <- env_data[data$station_name == metadata_Tw$receiver[1],1]
for (i in 1:n) {
    telemetry_data$Tw <- replace(telemetry_data$Tw, telemetry_data$station_name == metadata$receiver[i],unlist(env_data[telemetry_data$station_name == metadata$receiver[i],i]))
}
return(telemetry_data$Tw)

}