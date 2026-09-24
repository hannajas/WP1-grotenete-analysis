# Function to calculate the station distance from a 'source' station. It is based on an 'under the hood' function in Hugo Flavio's actel package
# https://github.com/hugomflavio/actel
# For more questions, ask Hugo
# hflavio@wlu.ca



distanceSource <- function(movements, distance.method, dist.mat) {
  #appendTo("debug", "Running movementSpeeds.")
  movements$distance_to_source_m[1] <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[1] & all(!grep("^Unknown$", movements$station_name[(1):i]))) {
        if (distance.method == "last to first"){
          dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[1])]
        }
        movements$distance_to_source_m[i] <<- round(dist, 6)
        rm(dist)
      } else {
        movements$speed_m_s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}



