data_onset <- data %>%
  group_by(tag_serial_number) %>%
  filter(row_number() == first_migratory_idx[1])

data_onset$solar <- solartime(
  data_onset$departure,
  lon = data_onset$deploy_longitude,
  lat = data_onset$deploy_latitude,
  tz = 0
)$solar
bins <- 24
data_onset$solar_binned <- cut(
  data_onset$solar,
  breaks = seq(0, 2 * pi, by = 2 * pi / bins),
  include.lowest = TRUE,
  labels = FALSE
)
data_onset$solar_binned <- data_onset$solar_binned * (2 * pi / bins)

#in figure:
# scale_x_continuous(
# breaks = seq(0, 2 * pi - bin_width, by = pi / 4),
# labels = parse(text = c("0", "pi/4", "pi/2", "3*pi/4", "pi","5*pi/4", "3*pi/2", "7*pi/4")),
# limits = c(0, 2 * pi) # ensure full circle
# ) +
