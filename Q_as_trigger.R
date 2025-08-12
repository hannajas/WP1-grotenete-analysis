library(tidyverse)
library(dplyr)

#Q trigger voor eerste migratie?
# ik weet wanneer ze migreren

#
variable <- "Q"
style <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_blank(),
  axis.title.x = element_text(size = 25),
  axis.text.y = element_text(size = 25, colour = "black"),
  axis.title.y = element_text(size = 25)
)
# load data
data_env_int <- read.csv(
  "./data/interim/migration_env_inter_test.csv",
  header = TRUE,
  sep = ","
)
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

# source functions
source("./src/align_resolutions_function.R")
source("./src/inverse_distance_function.R")

list <- split(data_env_int, data_env_int$tag_serial_number) # lijst van alle eels
# identificeer het eerste knikpunt (tijdstip t_k) op plaats x_k ("migratory for the first time")
#empty dataframe
datatemp <- data.frame()

for (i in 1:length(list)) {
  data_eel <- list[[i]] # neem de eerste eel
  #error message if to little detection points
  data_eel$date <- as.POSIXct(data_eel$date, tz = "UTC") # zorg dat de datum in het juiste formaat is
  index <- which(data_eel$cluster == 2)[1] # index van het eerste knikpunt
  if (nrow(data_eel) < 2) {
    next
    #stop("Not enough detection points for this eel.")
  } else if (index == 1 | is.na(index)) {
    next
    #stop("Eel started as migratory.")
  }
  t_k <- data_eel$date[index]
  x_k <- data_eel$distance_to_source_m[index]

  # identificeer een range [t_k - r; t_k]
  range <- as.period(1, "days")
  # Interpoleer de Q's naar punt x_k (Q_{inter,k})
  #data_eel$distance_to_source_m <- x_k #DUS dit is super schaalbar naar v!!

  env_data_Q <- align_resolutions_function(
    variable,
    as.difftime(5, units = "mins"), #as.period(5, "mins"),
    metadata,
    upsample_method = "ffill",
    data_eel
  )
  Q_inter_k <- inverse_distance(
    data_eel,
    env_data_Q,
    metadata,
    1,
    variable
  )
  deltaQ_inter_k <- Q_inter_k - lag(Q_inter_k)

  #proberen met V ipv Q
  # Q_inter_k <- read_csv(
  #   "./data/interim/processed/gnt07a_1066_V.csv"
  # )

  #Bereken vergelijk de Q_max binnen de range met de Q_max binnen [t_release; t_k - r]
  Q_trigger <- max(Q_inter_k[
    data_eel$date > t_k - as.duration(range) &
      data_eel$date <= t_k
  ])
  Q_rest <- Q_inter_k[data_eel$date < t_k - as.duration(range)]

  deltaQ_trigger <- mean(deltaQ_inter_k[
    data_eel$date > t_k - as.duration(range) &
      data_eel$date <= t_k
  ])
  deltaQ_rest <- deltaQ_inter_k[data_eel$date < t_k - as.duration(range)]

  #create data frame
  data_plot <- data.frame(
    Q = c(Q_rest, Q_trigger),
    tag_serial_number = data_eel$tag_serial_number[1],
    type = c(
      rep("Q_rest", length(deltaQ_rest)),
      rep("trigger", length(deltaQ_trigger))
    )
  )
  #marge with datatemp
  datatemp <- rbind(datatemp, data_plot)
}

dataplot <- datatemp[datatemp$type == "Q_rest", ]
trigger_vals <- datatemp[
  datatemp$type == "trigger",
  c("tag_serial_number", "Q")
]
#boxplot Qrest and plot Q_trigger with ggplot2
g <- ggplot(data = dataplot) +
  geom_boxplot(aes(y = Q)) +
  geom_hline(
    data = trigger_vals,
    aes(yintercept = Q, group = tag_serial_number),
    color = "red"
  ) +
  facet_wrap(~tag_serial_number) +
  style + #other text on y axis
  labs(y = "Q (m^3/s)")
plot(g)
ggsave(
  "./figures/as_trigger/absTw_trigger_rangemax_1day.png",
  plot = g,
)

#plot Q_trigger
# p <- ggplot() +
#   geom_line(aes(x = data_eel$date, y = Q_inter_k), color = "blue") +
#   geom_vline(xintercept = t_k, linetype = "dashed", color = "red") +
#   labs(
#     title = paste("Q values for", data_eel$tag_serial_number[1]),
#     x = "Date",
#     y = "Q values"
#   ) +
#   theme_minimal()
