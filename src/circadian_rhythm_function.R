#write a function date as input and returns "dusk", "dawn", "night" and "day"
circadian_rhythm <- function(date, circadian) {
  # Get the circadian data for the given date
  circadian$date <- ymd_hms(circadian$date,truncated=3)
  n_row <- which(circadian$date == floor_date(date, unit = "day"))
  
  # Get the time of day for the given date
  time_of_day <- ymd_hms(date, truncated = 3)
  
  # Determine the circadian rhythm based on the time of day
  if (time_of_day >= circadian$nightEnd[n_row] & time_of_day < circadian$sunrise[n_row]) {
    return("dawn")
  } else if (time_of_day >= circadian$sunrise[n_row] & time_of_day < circadian$sunset[n_row]) {
    return("day")
  } else if (time_of_day >= circadian$sunset[n_row] & time_of_day < circadian$night[n_row]) {
    return("dusk")
  } else {
    return("night")
  }
}