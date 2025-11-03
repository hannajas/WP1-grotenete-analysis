inverse_distance <- function(
  telemetry_data,
  env_data,
  metadata,
  p,
  data_type
) {
  metadata_filter <- filter(metadata, stringr::str_detect(metadata$type, data_type))

  n <- dim(metadata_filter)[1]
  V <- as.matrix(env_data)
  nan_V <- which(is.nan(V))

  if (data_type == "R") {
    W <- lapply(1:n, function(i) {
      distance <- distm(
        telemetry_data[c("inter_longitude", "inter_latitude")],
        metadata_filter[c("station_longitude", "station_latitude")][i, ],
        fun = distHaversine
      )
      ifelse((distance == 0), NaN, abs(1 / (distance)^p))
    })
  } else {
    # find two closest recievers (upstream and downstream)

    W <- lapply(seq_along(telemetry_data$interpolation_location), function(j) {
      diffs <- metadata_filter$distance_to_source -
        telemetry_data$interpolation_location[j]
      diffs_refect <- -(abs(metadata_filter$distance_to_source - dist_split) +
        abs(dist_split - telemetry_data$interpolation_location[j]))
      if (telemetry_data$inter_segment[j] == "zes_up") {
        diffs[metadata_filter$segment == "zes_down"] <- diffs_refect[
          metadata_filter$segment == "zes_down"
        ]
      } else if (telemetry_data$inter_segment[j] == "zes_down") {
        diffs[metadata_filter$segment == "zes_up"] <- diffs_refect[
          metadata_filter$segment == "zes_up"
        ]
      }
      # upstream (closest negative diff) and downstream (closest positive diff)
      upstream_idx <- if (any(diffs < 0)) which.max(diffs[diffs < 0]) else NA
      downstream_idx <- if (any(diffs > 0)) which.min(diffs[diffs > 0]) else NA

      selected_idx <- na.omit(c(
        if (!is.na(upstream_idx)) which(diffs == max(diffs[diffs < 0]))[1],
        if (!is.na(downstream_idx)) which(diffs == min(diffs[diffs > 0]))[1], #of NA for upstream_idx or downstream_idx is NA --> choose closest environmental station
        if (is.na(upstream_idx) | is.na(downstream_idx)) which.min(abs(diffs)) #if no upstream or downstream station, choose closest station
      ))

      w <- rep(0, length(diffs)) # start with zeros
      if (length(selected_idx) == 2) {
        w[selected_idx] <- 1 / abs(diffs[selected_idx])^p
      } else if (length(selected_idx) == 1) {
        w[selected_idx] <- 1
      }
      return(w)
    })
  }

  W <- matrix(unlist(W), ncol = n, byrow = TRUE)

  # dealing with the nan values in temperature values
  W[nan_V] <- 0

  if (data_type == "Q" | data_type == "V"| data_type == "Q_an") {
    #enkel gewicht als binnen zelfde rivier segment
    ind_row_gn <- which(telemetry_data$inter_segment == "gn")
    ind_col_gn <- which(metadata_filter$segment != "gn")
    ind_row_rup <- which(telemetry_data$inter_segment == "rup")
    ind_col_rup <- which(metadata_filter$segment != "rup")
    ind_row_zes <- which(
      telemetry_data$inter_segment == "zes_up" |
        telemetry_data$inter_segment == "zes_down"
    )
    ind_col_zes <- which(metadata_filter$segment != "zes")
    W[ind_row_gn, ind_col_gn] <- 0
    W[ind_row_rup, ind_col_rup] <- 0
    W[ind_row_zes, ind_col_zes] <- 0
  }
  telemetry_data$temp <- rowSums(V * W, na.rm = TRUE) /
    rowSums(W, na.rm = TRUE)

  for (i in 1:n) {
    #voor waarde bij release_location (hier ligt ook een Tw en Q meting)
    if (!is.na(metadata_filter$receiver[i])) {
      find_receivers <- telemetry_data$station_name ==
        metadata_filter$receiver[i]
      find_receivers <- replace(
        #replace NA to FALSE
        find_receivers,
        is.na(find_receivers),
        FALSE
      )
      telemetry_data$temp <- replace(
        telemetry_data$temp,
        find_receivers,
        unlist(env_data[
          telemetry_data$station_name == metadata_filter$receiver[i],
          i
        ])
      )
    }
  }
  return(telemetry_data$temp)
}
