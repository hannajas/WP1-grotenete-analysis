get_nearest_stations <- function(.distance_stations, limit) {
  stations <- colnames(.distance_stations)
  return(stations[which(.distance_stations < limit & 
                          .distance_stations > 0)])
}