# Function to calculate the water velocity at 5 location with Q and cross-section area data
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

get_velocity <- function(Q_station_name, H_station_name, metadata) {
  #bv. "gnt07_1066", "gnt07_1066"
  # Load the data
  metadata_Q <- filter(metadata, metadata$type == "Q")
  metadata_H <- filter(metadata, metadata$type == "H")
  path <- paste(
    "C:/Code/WP1-grotenete-analysis/data/interim/processed/",
    H_station_name,
    "_H.csv",
    sep = ""
  )
  level <- read_csv(path, show_col_types = FALSE) %>%
    rename(level = "Value")
  #assign(paste(H_station_name,'_H', sep =""), temp)

  path <- paste("./data/interim/processed/", Q_station_name, "_Q.csv", sep = "")
  discharge <- read_csv(path, show_col_types = FALSE) %>%
    rename(Q = "Value")
  #assign(paste(Q_station_name,'_Q', sep =""), temp)

  path <- paste(
    'C:/Code/WP1-grotenete-analysis/data/external/cross_section_H_A/',
    H_station_name,
    '.csv',
    sep = ""
  )
  CS <- read_csv(path, show_col_types = FALSE) %>%
    rename(A = "crosssection area")
  #assign(paste(H_station_name,'_CS', sep =""), temp)
  # fit H-A curve
  fit <- lm(A ~ poly(level, 2, raw = TRUE), data = CS)
  if (
    metadata_H$resolution[metadata_H$name == H_station_name] ==
      metadata_Q$resolution[metadata_Q$name == Q_station_name]
  ) {
    level$A <- predict(fit, newdata = level)
    V <- full_join(discharge, level, by = "Timestamp") %>%
      mutate(V = Q / A) %>%
      select(Timestamp, V) %>%
      rename(Value = "V")
  } else {
    # H data has higher resolution than Q data
    discharge$level <- unlist(lapply(
      1:dim(discharge)[1],
      function(i, x) {
        mean(
          x$level[
            x$Timestamp >= discharge$Timestamp[i] &
              x$Timestamp < discharge$Timestamp[i + 1]
          ],
          na.rm = TRUE
        )
      },
      x = level
    ))
    discharge$A <- predict(fit, newdata = discharge)
    V <- discharge %>%
      mutate(V = Q / A) %>%
      select(Timestamp, V) %>%
      rename(Value = "V")
  }
  return(V)
}
