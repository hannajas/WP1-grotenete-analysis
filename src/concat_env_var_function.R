# Function to to link one environmental variable to the processed telemetry data
concat_env_var <- function(telemetry_data, metadata, env_data, data_type) {
  #dep_time <- round_date(telemetry_data$departure,unit = metadata$resolution[1])
  #arr_time <- round_date(telemetry_data$arrival,unit = metadata$resolution[1])
  # calculate the mean temperature between arrival an departure
  data_list <- split(telemetry_data, f = telemetry_data$tag_serial_number)

  test <- lapply(data_list, function(a) {
    a$temp <- unlist(lapply(
      1:dim(a)[1],
      function(i, x) {
        beg_time <- round_date(
          a$departure[i - 1] - a$residence[i - 1] / 2,
          unit = metadata$resolution[1]
        )
        end_time <- round_date(
          a$arrival[i] + a$residence[i] / 2,
          unit = metadata$resolution[1]
        )
        if (data_type == "R") {
          return(median(
            x$Value[x$Timestamp >= beg_time & x$Timestamp <= end_time],
            na.rm = TRUE
          ))
        } else {
          return(mean(
            x$Value[x$Timestamp >= beg_time & x$Timestamp <= end_time],
            na.rm = TRUE
          ))
        }
      },
      x = env_data
    ))
    if (data_type == "R") {
    a$temp[1] <- median(env_data$Value[
      env_data$Timestamp ==
        round_date(a$departure[1], unit = metadata$resolution[1])
    ])} else {
      a$temp[1] <- mean(env_data$Value[
        env_data$Timestamp ==
          round_date(a$departure[1], unit = metadata$resolution[1])
      ])
    }
    return(a)
    #print(paste("a$temp: ",length(a$temp)))
  })
  test_df <- plyr::ldply(test, data.frame)
  #print(paste(test_df$temp))
  #telemetry_data$temp <- unlist(lapply(1:length(dep_time), function(i,x) {
  #    mean(x$Value[x$Timestamp >= dep_time[i-1]-telemetry_data$residence[i-1]/2 & x$Timestamp <= arr_time[i]+telemetry_data$residence[i]/2], na.rm=TRUE) }, x = env_data))
  #return(telemetry_data$temp)
  return(test_df$temp)
}
