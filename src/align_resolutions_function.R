library(dplyr)
library(readr)
library(lubridate)
library(xts)
library(zoo)

align_resolutions_function <- function(
  data_type,
  resolution,
  metadata,
  upsample_method = "ffill",
  tel_inter_data = NULL
) {
  metadata_filter <- filter(metadata, type == data_type)
  all_aligned_data <- list()
  tel_inter_data_temp <- tel_inter_data

  for (i in seq_len(nrow(metadata_filter))) {
    path <- paste0(
      './data/interim/processed/',
      metadata_filter$name[i],
      '_',
      data_type,
      '.csv',
      sep = ""
    )
    env_data <- read_csv(path, show_col_types = FALSE)

    # Ensure timestamp is parsed and sorted
    env_data <- env_data %>%
      mutate(Timestamp = ymd_hms(Timestamp, truncated = 3)) %>%
      arrange(Timestamp) # %>%
    #select(-...1)

    env_data <- env_data %>%
      mutate(across(-Timestamp, as.numeric))
    # Convert to xts
    env_xts <- xts(
      env_data[, -which(names(env_data) == "Timestamp")],
      order.by = env_data$Timestamp
    )

    #env_xts <- xts(env_data, order.by = env_data$Timestamp)

    original_resolution <- period_to_seconds(as.period(metadata_filter$resolution[
      i
    ])) #as.numeric(metadata_filter$resolution[i],units = "secs")
    target_resolution <- as.numeric(resolution, units = "secs")

    # Create target time index
    full_time_index <- seq(
      from = start(env_xts),
      to = end(env_xts),
      by = target_resolution
    )

    if (original_resolution > target_resolution) {
      # Upsampling
      env_xts_resampled <- xts::merge.xts(env_xts, xts(, full_time_index))

      if (upsample_method == "linear") {
        env_xts_filled <- na.approx(env_xts_resampled, na.rm = FALSE)
      } else if (upsample_method == "ffill") {
        env_xts_filled <- na.locf(env_xts_resampled, na.rm = FALSE)
      } else {
        stop("Unsupported upsample_method. Choose 'linear' or 'ffill'.")
      }
    } else {
      # Downsampling
      env_xts_filled <- period.apply(
        env_xts,
        endpoints(env_xts, on = "secs", k = target_resolution),
        colMeans,
        na.rm = TRUE
      )
    }

    # Back to dataframe
    aligned_df <- data.frame(
      timestamp = index(env_xts_filled),
      coredata(env_xts_filled)
    )
    #all_aligned_data[[i]] <- aligned_df
    aligned_df <- aligned_df %>%
      rename(!!metadata_filter$name[i] := Value)

    result <- left_join(
      tel_inter_data_temp,
      aligned_df[, c("timestamp", metadata_filter$name[i])],
      by = c("date" = "timestamp")
    )
    tel_inter_data_temp <- result
  }
  return(result[, metadata_filter$name])
}
