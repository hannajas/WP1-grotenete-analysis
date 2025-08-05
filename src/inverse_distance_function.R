inverse_distance <- function(telemetry_data, env_data, metadata, p, data_type) {
  metadata_filter <- filter(metadata, metadata$type == data_type)

  n <- dim(metadata_filter)[1]
  V <- as.matrix(env_data)
  nan_V <- which(is.nan(V))

  if (data_type == "R") {
    W <- lapply(1:n, function(i) {
      distance <- distm(
        telemetry_data[c("deploy_longitude", "deploy_latitude")],
        metadata[c("station_longitude", "station_latitude")][i, ],
        fun = distHaversine
      )
      ifelse((distance == 0), NaN, abs(1 / (distance)^p))
    })
  } else {
    W <- lapply(1:n, function(i) {
      ifelse(
        (telemetry_data$distance_to_source_m -
          metadata_filter$distance_to_source[i]) ==
          0,
        NaN,
        abs(
          1 /
            (telemetry_data$distance_to_source_m -
              metadata_filter$distance_to_source[i])^p
        )
      )
    })
  }

  W <- matrix(unlist(W), ncol = n)

  # dealing with the nan values in temperature values
  W[nan_V] <- 0

  if (data_type == "Q") {
    grenswaardes_distance_to_source <- c(43106.96, 70306.3) #boundaries between de different river segments (Grote Nete, Rupel, Schelde)
    telemetry_data$river_segment <- "rup"
    telemetry_data$river_segment[
      telemetry_data$distance_to_source_m < grenswaardes_distance_to_source[1]
    ] <- "gn"
    telemetry_data$river_segment[
      telemetry_data$distance_to_source_m > grenswaardes_distance_to_source[2]
    ] <- "zes"

    #spelen met W
    ind_row_gn <- which(telemetry_data$river_segment == "gn")
    ind_col_gn <- which(metadata_filter$segment != "gn")
    ind_row_rup <- which(telemetry_data$river_segment == "rup")
    ind_col_rup <- which(metadata_filter$segment != "rup")
    ind_row_zes <- which(telemetry_data$river_segment == "zes")
    ind_col_zes <- which(metadata_filter$segment != "zes")
    W[ind_row_gn, ind_col_gn] <- 0
    W[ind_row_rup, ind_col_rup] <- 0
    W[ind_row_zes, ind_col_zes] <- 0
  }
  show_W <<- W
  telemetry_data$temp <- rowSums(V * W, na.rm = TRUE) / rowSums(W, na.rm = TRUE)

  # for (i in 1:n) {
  #   if (!is.na(metadata_filter$receiver[i])) {
  #     find_receivers <- telemetry_data$station_name ==
  #       metadata_filter$receiver[i]
  #     find_receivers <- replace(
  #       find_receivers,
  #       is.na(find_receivers),
  #       FALSE
  #     )
  #     telemetry_data$temp <- replace(
  #       telemetry_data$temp,
  #       find_receivers,
  #       unlist(env_data[
  #         telemetry_data$station_name == metadata_filter$receiver[i],
  #         i
  #       ])
  #     )
  #   }
  # }
  return(telemetry_data$temp)
}
