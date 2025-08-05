#Q trigger voor eerste migratie?

# load data
data_env_int <- read.csv(
  "./data/interim/migration_env_inter.csv",
  header = TRUE,
  sep = ","
)
metadata <- read_csv('./data/interim/metadata.csv', show_col_types = FALSE)

# source functions
source("./src/align_resolutions_function.R")
source("./src/inverse_distance_function.R")

list <- split(data_env_int, data_env_int$tag_serial_number) # lijst van alle eels
# identificeer het eerste knikpunt (tijdstip t_k) op plaats x_k ("migratory for the first time")
data_eel <- list[[5]] # neem de eerste eel
#error message if to little detection points
data_eel$date <- as.POSIXct(data_eel$date, tz = "UTC") # zorg dat de datum in het juiste formaat is
index <- which(data_eel$cluster == 2)[1] # index van het eerste knikpunt
if (nrow(data_eel) < 2) {
  stop("Not enough detection points for this eel.")
} else if (index == 1) {
  stop("Eel started as migratory.")
}
t_k <- data_eel$date[index]
x_k <- data_eel$distance_to_source_m[index]

# identificeer een range [t_k - r; t_k]
range <- as.period(2, "days")
# Interpoleer de Q's naar punt x_k (Q_{inter,k})
data_eel$distance_to_source_m <- x_k

env_data_Q <- align_resolutions_function(
  "Q",
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
  "Q"
)
deltaQ_inter_k <- Q_inter_k - lag(Q_inter_k)

# Bereken vergelijk de Q_max binnen de range met de Q_max binnen [t_release; t_k - r]
Q_trigger <- max(Q_inter_k[
  data_eel$date > t_k - as.duration(range) &
    data_eel$date <= t_k + as.duration(range)
])
Q_rest <- Q_inter_k[data_eel$date < t_k - as.duration(range)]

deltaQ_trigger <- max(deltaQ_inter_k[
  data_eel$date > t_k - as.duration(range) &
    data_eel$date <= t_k + as.duration(range)
])
deltaQ_rest <- deltaQ_inter_k[data_eel$date < t_k - as.duration(range)]

#boxplot Qrest and plot Q_trigger with ggplot2
g <- ggplot() +
  geom_boxplot(aes(x = "Q_rest", y = deltaQ_rest)) +
  geom_hline(aes(yintercept = deltaQ_trigger), color = "red") +
  labs(title = data_eel$tag_serial_number[1], x = "Q values", y = "Values") +
  theme_minimal()
plot(g)
