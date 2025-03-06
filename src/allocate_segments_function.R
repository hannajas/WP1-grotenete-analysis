allocate_segments <- function(ltraj_with_segments, data_onetraj){
  n_seg <- length(ltraj_with_segments)
  data_onetraj$segment <- NA
  for(i in 1:n_seg){
    date <- ltraj_with_segments[[i]]["date"]
    data_onetraj$segment[data_onetraj$middledate %in% date$date] <- i
  }
  return(data_onetraj)
}