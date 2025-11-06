# Comparing the amount of detections in function of the tidal rhythm (similar as in Keirsebelik et al. 2025)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be
get_idx_weights <- function(telemetry_data, metadata_filter, dist_split) {
    diffs <- telemetry_data$distance_to_source_m -
    metadata_filter$distance_to_source
  diffs_refect <- -(abs(metadata_filter$distance_to_source - dist_split) +
    abs(dist_split - telemetry_data$distance_to_source_m))
  if (telemetry_data$inter_segment == "zes_up") {
    diffs[metadata_filter$segment == "zes_down"] <- diffs_refect[
      metadata_filter$segment == "zes_down"
    ]
  } else if (telemetry_data$inter_segment == "zes_down") {
    diffs[metadata_filter$segment == "zes_up"] <- diffs_refect[
      metadata_filter$segment == "zes_up"
    ]
  }
  upstream_idx <- if (any(diffs < 0)) which.max(diffs[diffs < 0]) else NA
  downstream_idx <- if (any(diffs > 0)) which.min(diffs[diffs > 0]) else NA

  selected_idx <- na.omit(c(
    if (!is.na(upstream_idx)) which(diffs == max(diffs[diffs < 0]))[1],
    if (!is.na(downstream_idx)) which(diffs == min(diffs[diffs > 0]))[1], #of NA for upstream_idx or downstream_idx is NA --> choose closest environmental station
    if (is.na(upstream_idx) | is.na(downstream_idx)) which.min(abs(diffs)) #if no upstream or downstream station, choose closest station
  ))

  w <- rep(0, length(diffs)) # start with zeros
  #weights should sum to one
  if (length(selected_idx) == 2) {
    w[selected_idx] <- 1 / abs(diffs[selected_idx])^p
  } else if (length(selected_idx) == 1) {
    w[selected_idx] <- 1
  }
  return(list(selected_idx, w))
}

get_weighted_tide <- function(selected_idx, w, metadata_tij) {
  tz_use <- attr(x$Timestamp, "tzone")
  if (is.null(tz_use) || tz_use == "") {
    tz_use <- "UTC"
  } #get your weighted tide
  if (length(selected_idx) == 1) {
    weighted <- get(paste(metadata_tij$name[selected_idx], '_tij', sep = "")) #interval as interval
  } else {
    x <- get(paste(metadata_tij$name[selected_idx[1]], '_tij', sep = "")) %>%
      mutate(w = w[selected_idx[1]])
    y <- get(paste(metadata_tij$name[selected_idx[2]], '_tij', sep = "")) %>%
      mutate(w = w[selected_idx[2]])
    test <- difference_inner_join(
      x,
      y,
      by = "Timestamp",
      max_dist = hours(2)
    ) %>%
      mutate(
        Timestamp = lubridate::as_datetime(
          (as.numeric(Timestamp.x) * w.x + as.numeric(Timestamp.y) * w.y) /
            (w.x + w.y),
          tz = tz_use
        ),
        Value = (Value.x * w.x + Value.y * w.y) / (w.x + w.y),
        tij = ifelse(w.x >= w.y, tij.x, tij.y),
        interval = Timestamp %--% lead(Timestamp),
        interval_sec = int_length(interval)
      ) %>%
      select(Timestamp, Value, tij, interval, interval_sec)
    weighted <- test
  }
}
