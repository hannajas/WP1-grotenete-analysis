# Evaluating the calculated velocities (from cross_sections/get_velocities)
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

data_eels <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)

ggplot(data_eels, aes(x = Q, y = V)) +
  geom_point() +
  labs(title = "Arrival vs Departure", x = "Q", y = "V") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )

env_data <- read_csv(
  paste('./data/interim/processed/', 'zes01a_1066', '_V.csv', sep = ""),
  show_col_types = FALSE
)
ggplot(env_data, aes(x = Timestamp, y = Value)) +
  geom_line() +
  labs(title = "Vw values", x = "Timestamp", y = "Vw values") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  )
