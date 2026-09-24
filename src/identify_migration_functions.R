#' Functions for identify eel downstream migration periods
#'
#' Damiano Oldoni (damiano.oldoni@inbo.be) Pieterjan Verhelst
#' (pieterjan.verhelst@inbo.be)
#'
#' R script with functions used in identify_migration.R script for identifying
#' eel migration behavior based on discussion in
#' https://github.com/PieterjanVerhelst/eel-meta-analysis/issues/17

library(assertthat) # to declare the conditions the code should satisfy
library(lubridate)  # to work with datetime
library(dplyr)      # to do data wrangling
library(tidylog)    # to get useful messages about dplyr and tidyr operations


#' This function returns the very first value of a vector(and corresponding
#' index) above a certain threshold. NA is returned if there are no values above
#' threshold.
#' 
#' @param x A numeric vector
#' @param threshold A numeric threshold
#' 
#' @return A list with two slots: `value` and `index`
#' @examples
#' # example 1: there are values above threshold
#' v <- c(1,3,5,2)
#' first_min(x = v, threshold = 3)
#' 
#' # example 2: no values above threshold
#' w <- c(1,3, 4)
#' first_min(x = w, threshold = 4)
first_min <- function(x, threshold) {
  value <- x[x > threshold][1]
  if (!is.na(value)) {
    index <- min(which(x > threshold))
  } else {
    index <- NA
  }
  return(list(value = value,
              index = index))
}

#' Function to get migration status at a specific row
#' 
#' @param df A data.frame
#' @param row_idx A positive number which is used as row index
get_migration <- function(df, row_idx, dist_threshold, speed_threshold) {
  
  ## check inputs
  # df is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # we make use of a column in df. So, it must be present
  assertthat::assert_that("distance_to_source_m" %in% names(df),
                          msg = "Column `distance_to_source_m` not found in df."
  )
  assertthat::assert_that(is.integer(row_idx),
                          msg = "row_idx must be an integer."
  )
  assertthat::assert_that(row_idx > 0,
                          msg = "row_idx must be positive."
  )
  # dist_threshold is a number
  assertthat::assert_that(is.numeric(dist_threshold))
  # speed_threshold is a number
  assertthat::assert_that(is.numeric(speed_threshold))
  # dist_threshold should typically must be strictly positive
  assertthat::assert_that(dist_threshold > 0,
                          msg = "dist_threshold must be strictly positive")
  # speed_threshold should typically must be strictly positive
  assertthat::assert_that(speed_threshold > 0,
                          msg = "speed_threshold must be strictly positive")
  
  ## start core function
  distance_to_source_m <- df$distance_to_source_m
  l <- length(distance_to_source_m)
  #get the very first value/index above distance threshold
  first_value <- first_min(
    distance_to_source_m[(row_idx + 1):l], 
    threshold = distance_to_source_m[row_idx] + dist_threshold)
  first_value$index <- first_value$index + row_idx
  return(first_value)
}

#' Core function to find migration periods
#'
#' @param df A data.frame with migration data. Next columns must be present:
#' - `distance_to_source_m`
#' - `arrival`
#' @param dist_threshold A numeric value used as the minimum value to calculate the speed of the eel.
#' @param speed_threshold A numeric value: only periods with speed above this value are flagged as migration. 
#' @param smooth_threshold A numeric value: a threshold to avoid migration starts during a stationary phase.
get_migrations <- function(df, 
                           dist_threshold, 
                           speed_threshold,
                           smooth_threshold) {
  ## check inputs
  # df is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # dist_threshold is a number
  assertthat::assert_that(is.numeric(dist_threshold))
  # speed_threshold is a number
  assertthat::assert_that(is.numeric(speed_threshold))
  # we make use of some columns in df. So, they need to be present in df
  assertthat::assert_that("distance_to_source_m" %in% names(df),
                          msg = "Column `distance_to_source_m` not found in df."
  )
  assertthat::assert_that("arrival" %in% names(df),
                          msg = "Column `arrival` not found in df."
  )
  
  ## set downstream_migration to FALSE if eel is swimming upstream
  df <-
    df %>%
    mutate(distance_to_next = lead(distance_to_source_m)) %>%
    mutate(downstream_migration = if_else(
      distance_to_source_m > distance_to_next,
      FALSE,
      NA)) %>%
    select(-distance_to_next)
  
  ## apply speed threshold algorithm point-wise
  df$first_dist_to_use <- NA
  df$first_dist_to_use_idx <- NA
  df$time_first_dist_to_use <- NA_POSIXct_
  for (i in seq_len(nrow(df))) {
    if (is.na(df$downstream_migration[i])) {
      first_dist_to_use <- get_migration(df, i, dist_threshold, speed_threshold)
      df$first_dist_to_use[i] <- first_dist_to_use$value
      df$first_dist_to_use_idx[i] <- first_dist_to_use$index
      df$time_first_dist_to_use[i] <- df$arrival[first_dist_to_use$index]
    }
  }
  
  # calculate speed and apply speed threshold to assess downstream migration
  df <- df %>%
    mutate(delta_totdist = if_else(is.na(downstream_migration), 
                                   first_dist_to_use - distance_to_source_m, 
                                   NA)) %>%
    mutate(delta_t = if_else(
      is.na(downstream_migration), 
      as.numeric(as.duration(time_first_dist_to_use - arrival)), 
      NA)) %>%
    mutate(migration_speed = if_else(is.na(downstream_migration), 
                                     delta_totdist / delta_t, 
                                     NA)) %>%
    mutate(downstream_migration = is.na(
      downstream_migration) & migration_speed >= speed_threshold
    )
  
  # avoid starting downstream migrations with a stationary phase
  df <-
    df %>%
    mutate(distance_to_next = lead(distance_to_source_m)) %>%
    mutate(downstream_migration = if_else(
      downstream_migration == TRUE & 
        distance_to_next > (distance_to_source_m + smooth_threshold),
      TRUE,
      FALSE)) %>%
    select(-distance_to_next)
  
  # add column flagging the general migration process from very first start to
  # its very last end
  df$migration <- FALSE # initialization
  first_true <- which(df$downstream_migration == TRUE)[1]
  last_true <- which(
    df$distance_to_source_m == max(df$distance_to_source_m, na.rm = TRUE)
  )[1]
  if (!is.na(first_true) & !is.na(last_true)) {
    df$migration[first_true:last_true] <- TRUE
  }
  return(df)
}
