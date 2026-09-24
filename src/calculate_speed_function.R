# Function to calculate movement speed between consecutive stations. It is based on an 'under the hood' function in Hugo Flavio's actel package

movementSpeeds <- function(movements, dist.mat) {
  movements$swimtime_s[1] <- NA
  movements$swimdistance_m[1] <- NA
  movements$speed_m_s[1] <- NA
  
  movements$residence <- as.period(movements$arrival %--% movements$departure, unit="seconds")
  movements$residence_s <- as.numeric(movements$residence, units = "seconds")

  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[i - 1] & all(!grep("^Unknown$", movements$station_name[(i - 1):i]))) {

        a.sec <- as.vector(difftime(movements$arrival[i], movements$departure[i - 1], units = "secs"))
        my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        a.sec.adj <- as.vector(round(a.sec, 6)+ movements$residence_s[i]/2 + movements$residence_s[i-1]/2)
        movements$swimtime_s[i] <<- round(a.sec, 6)+ movements$residence_s[i]/2 + movements$residence_s[i-1]/2
        movements$swimdistance_m[i] <<- round(my.dist, 6)
        movements$speed_m_s[i] <<- round(my.dist/a.sec.adj, 6)
        rm(a.sec, my.dist)
      } else {
        movements$speed_m_s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}
