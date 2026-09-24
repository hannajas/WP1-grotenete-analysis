#' Retrieve smooth eel tracking time series along several stations.
#' 
#' This function smoothes an eel tracking time series along several stations (receivers).
#' @param df (dataframe) contains at least the following columns: 
#' \itemize{
#'  \item{`date_time`}
#'  \item{`station_name`}
#'  \item{`tag_serial_number`}
#'  \item{`counts`}{: number of detections within 1 minute}
#' @param proxy_stations (list) contains a vector for each station
#' with the nearest stations
#' @param eel (character) a code identifying the transmitter implanted in the eel. 
#' @param limit (integer) time window for detection in seconds,
#' default = 3600 (1h)
#' @param verbose (logical) if TRUE detailed informations about smoothing are displayed
#' @return a dataframe with the following columns: `station_name`, `date_time`, 
#' `end`, `receiver_id`, `deploy_longitude`, `deploy_latitude`, 
#' `counts` (number of rows from `df` merged) and `tag_serial_number` (constant)
#' @export
#' @importFrom dplyr select bind_rows filter
#' @importFrom lubridate as_datetime
get_timeline <- function(df, 
                         proxy_stations, 
                         eel, 
                         limit = 3600, 
                         verbose = FALSE)  {
  assertthat::assert_that(all(c("date_time", "tag_serial_number", "station_name", "counts") %in% colnames(df)),
                          msg = paste("Column date_time, tag_serial_number, station_name and counts", 
                                      "have to be present in dataframe"))
  assertthat::assert_that(all(unique(df$station_name) %in% names(proxy_stations)),
                          msg = paste("there are one or more stations in df which",
                                      "are not present in proxy_stations."))
  df %<>% filter(tag_serial_number == eel)
  # find start
  t <- min(df$date_time)
  # find end of time series
  max_t <- max(df$date_time)
  # find first station
  args <- df %>% 
    filter(date_time  == t) %>% select(station_name, receiver_id, 
                                  deploy_longitude, deploy_latitude, counts, animal_project_code)
  station_name <-  args %>% select(station_name)
  station_name <-  as.character(station_name[nrow(station_name),1])
  receiver_id <- args %>% select(receiver_id)
  #network <- args %>% select(network)
  deploy_longitude <- args %>% select(deploy_longitude)
  deploy_latitude <- args %>% select(deploy_latitude)
  animal_project_code <- args %>% select(animal_project_code)
  counts <- sum(args$counts)
  # first row of final timeline (output)
  timeline <- data.frame(station_name = station_name, 
                         start = as_datetime(t, tz = "UTC"),
                         end = as_datetime(t, tz = "UTC"),
                         receiver_id = as.character(receiver_id[nrow(receiver_id),1]),
                         #network = as.character(network[nrow(network),1]),
                         animal_project_code = as.character(animal_project_code[nrow(animal_project_code),1]),
                         deploy_longitude = as.character(deploy_longitude[nrow(deploy_longitude),1]),
                         deploy_latitude = as.character(deploy_latitude[nrow(deploy_latitude),1]),
                         counts = counts,
                         stringsAsFactors = FALSE)
  # apply limit
  end <- t + limit
  while (t < max_t) {
    #get near stations
    near_stations <- proxy_stations[[station_name]]
    # find next time and station
    df %<>% filter(date_time > t)
    t <- min(df$date_time)
    new_args <- df %>% filter(date_time  == t)
    new_station <- new_args %>% select(station_name)
    # from df to character; if more than one station (more than one row)
    # at same timestamp choose the first one
    new_station <- as.character(new_station[nrow(new_station),1])
    # eel is at a new "not near" station or detected after time limit
    if ((!new_station %in% near_stations & new_station != station_name) |
        (t > end)) {
      station_name <- new_station
      receiver_id <- new_args %>% select(receiver_id)
      #network <- new_args %>% select(network)
      animal_project_code <- new_args %>% select(animal_project_code)
      deploy_longitude <- new_args %>% select(deploy_longitude)
      deploy_latitude <- new_args %>% select(deploy_latitude)
      counts <- sum(new_args$counts)
      near_stations <- proxy_stations[[station_name]]
      timeline %<>% bind_rows(
        data.frame(station_name = station_name,
        start = as_datetime(t, tz = "UTC"), 
        end = as_datetime(t, tz = "UTC"),
        receiver_id = as.character(receiver_id[nrow(receiver_id),1]),
        #network = as.character(network[nrow(network),1]),
        animal_project_code = as.character(animal_project_code[nrow(animal_project_code),1]),
        deploy_longitude = as.character(deploy_longitude[nrow(deploy_longitude),1]),
        deploy_latitude = as.character(deploy_latitude[nrow(deploy_latitude),1]),
        counts = counts,
        stringsAsFactors = FALSE))
      if (isTRUE(verbose)) {
        print(paste(eel, "is at", t, "at station:", station_name))
      }
    } else{
      counts <- counts + sum(new_args$counts)
      timeline[nrow(timeline),]$end <- as_datetime(t, tz = "UTC")
      timeline[nrow(timeline),]$counts <- counts
    }
    end <- t + limit
  }
  timeline$tag_serial_number <- eel
  return(tibble::as_tibble(timeline))
}
