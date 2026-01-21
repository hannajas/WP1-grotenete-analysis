library(bcpa)
library(ggplot2)
library(tidyverse)

data_eels <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE) %>%
    filter(tag_serial_number == "1171746" & !is.na(speed_m_s))

ggplot(data_eels, aes(x = arrival, y = speed_m_s)) +
    geom_line() +
    geom_point() +
    labs(title = "Eel Migration Data",
         x = "arrival time",
         y = "spped (m/s)") +
    theme(plot.title = element_text(hjust = 0.5))

# vrij weinig punten --> om aan classiifcatie te goei te doen best eerst een BB maar geen model,
#gwn een sampling