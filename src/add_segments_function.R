add_segments <- function(look_up, data, name_dist_col) {
  data <- data %>%
    rename(distance_to_source = name_dist_col)
  # add column to divide in segments: gn, rup, zes_up, zes_down
  boundaries <- look_up %>%
    group_by(NAAM) %>%
    summarise(max_distance = max(distance)) %>%
    ungroup()

  data$inter_segment <- NA
  data$inter_segment[
    data$distance_to_source < boundaries$max_distance[1]
  ] <- "gn"
  data$inter_segment[
    data$distance_to_source > boundaries$max_distance[2]
  ] <- "zes"
  data$inter_segment[
    data$distance_to_source <= boundaries$max_distance[2] &
      data$distance_to_source >= boundaries$max_distance[1]
  ] <- "rup"

  dist_af_splits <- 60363 #zeescheldt afwaartst, zeescheldt opwaarts

  data <- data %>%
    mutate(
      inter_segment = case_when(
        #klopt niet, je zit niet bij het station!
        inter_segment == "zes" & distance > dist_af_splits ~ "zes_down",
        inter_segment == "zes" ~ "zes_up",
        TRUE ~ inter_segment
      )
    )
  return(data$inter_segment)
}
