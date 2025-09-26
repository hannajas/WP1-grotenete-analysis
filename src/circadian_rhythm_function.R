# Function which calculates the circadian period ("dusk", "dawn", "night" and "day") based on the date
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

circadian_rhythm <- function(date, circadian, twilight) {
  # Get the circadian data for the given date
  circadian$date <- ymd_hms(circadian$date, truncated = 3)
  n_row <- which(circadian$date == floor_date(date, unit = "day"))

  # Get the time of day for the given date
  time_of_day <- ymd_hms(date, truncated = 3)

  # Determine the circadian rhythm based on the time of day
  if (
    time_of_day >= circadian$nightEnd[n_row] &
      time_of_day < circadian$sunrise[n_row]
  ) {
    if (twilight) {
      return("twilight")
    } else {
      return("dawn")
    }
  } else if (
    time_of_day >= circadian$sunrise[n_row] &
      time_of_day < circadian$sunset[n_row]
  ) {
    return("day")
  } else if (
    time_of_day >= circadian$sunset[n_row] &
      time_of_day < circadian$night[n_row]
  ) {
    if (twilight) {
      return("twilight")
    } else {
      return("dusk")
    }
  } else {
    return("night")
  }
}
