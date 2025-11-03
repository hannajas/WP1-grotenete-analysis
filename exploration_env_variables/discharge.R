# Load from /raw folder preprocess and write to /processed folder
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

# Load the environmental data
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_Q <- filter(metadata, metadata$type == "Q")
n <- dim(metadata_Q)[1]

for (i in 1:n) {
  path <- paste('./data/raw/discharge/', metadata_Q$name[i], '_Q.csv', sep = "")
  temp <- read_csv(path)
  assign(paste(metadata_Q$name[i], '_Q', sep = ""), temp)
}

# PRE_PROCESSING
rup00a_1066_Q$Timestamp <- floor_date(
  dmy_hms(rup00a_1066_Q$Timestamp, truncated = 3),
  "day"
)
zes00a_1066_Q$Timestamp <- floor_date(
  ymd_hms(zes00a_1066_Q$Timestamp, truncated = 3),
  "day"
)
zes29f_1066_Q$Timestamp <- floor_date(
  ymd_hms(zes29f_1066_Q$Timestamp, truncated = 3),
  "day"
)
for (i in 1:n) {
  path <- paste(
    './data/interim/processed/',
    metadata_Q$name[i],
    '_Q.csv',
    sep = ""
  )
  write.csv(get(paste(metadata_Q$name[i], '_Q', sep = "")), path)
}


###################################################################################################################
# CALCULATE ANOMALIE OF THE CLIMATOLOGY
# https://www.r-bloggers.com/2020/03/visualize-climate-anomalies/
# 1. climatology
library(wateRinfo)
#chat GPT
# Define measurement points
ts_id <- c(69694042, 67302010, 69196010, 75944010, 83735010, 67748010)
#ts_id <- 67302010
name <- c(
  "L10_077",
  "gnt07a_1066",
  "gnt05a_1066",
  "rup00a_1066",
  "zes29f_1066",
  "zes00a_1066"
)
#name <- "gnt07a_1066"
source_d <- c(1, 4,4,4,4,4) #datasource
#source_d <- 4
res <- c(15,15,5,60,60,60)
#res <- 15
source("./src/get_anomaly_functions.R")
###############################################################################################
# Periods
climatology_start <- 2005
climatology_end <- 2025
anomaly_start <- as_datetime("2019-01-01 00:00:00", tz = "UTC")
anomaly_end <- as_datetime("2021-02-28 23:59:59", tz = "UTC")
# ---- Main loop ----
for (i in seq_along(ts_id)) {
  message("Processing station: ", name[i])

  # 1️⃣ Download full record
  Q_full <- get_timeseries_chunked(
    ts_id[i],
    datasource = source_d[i],
    start_year = climatology_start,
    end_year = climatology_end,
    chunk_size = 1
  )

  if (nrow(Q_full) == 0) {
    message("⚠️ No data for ", name[i])
    next
  }
  #Q_full$Value <- scale(Q_full$Value) # convert to m3/s if needed
  # 2️⃣ Compute anomalies only for 2019–2021
  Q_an <- calc_anomaly(Q_full, anomaly_start, anomaly_end, res[i])

  write_csv(Q_an, paste0("./data/interim/processed/", name[i], "_Q_an.csv"))
}

#test climatology
dat_prep <- Q_full %>%
  mutate(doy = yday(Timestamp),
         year = year(Timestamp))
roll_mean <- rollmean(dat_prep$Value, 7*24*4, align = "center", fill = NA)
  
climatology <- dat_prep %>%  mutate(rollmean = roll_mean) %>%
  group_by(doy) %>%
  summarize(climatology = mean(rollmean, na.rm = TRUE), .groups = "drop")

df_an <- Q_full %>%
  filter(Timestamp >= anomaly_start, Timestamp <= anomaly_end) %>%
  mutate(doy = yday(Timestamp)) %>%
  left_join(climatology, by = "doy") %>%
  mutate(Value = Value - climatology)

ggplot(aes(x = doy, y = climatology), data = climatology) +
  geom_line() +
  labs(title = "Climatology with rolling mean (7 days)") +
  xlab("Day of Year") +
  ylab("Climatology Value") +
  theme_minimal()

ggplot() +
  geom_line(aes(x = Timestamp, y = Value), data = Tw_full, color = "blue", alpha = 0.5)