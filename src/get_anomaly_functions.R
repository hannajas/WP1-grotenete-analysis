# ---- Helper functions ----
library(zoo)
get_timeseries_chunked <- function(
  ts_id,
  datasource,
  start_year,
  end_year,
  chunk_size = 1
) {
  years_seq <- seq(start_year, end_year, by = chunk_size)

  intervals <- map2(
    years_seq,
    c(years_seq[-1] - 1, end_year),
    ~ c(.x, min(.y, end_year))
  )

  dfs <- map(intervals, function(intv) {
    from <- sprintf("%d-01-01", intv[1])
    to <- sprintf("%d-12-31", intv[2])
    message("Downloading ", ts_id, " from ", from, " to ", to)

    tryCatch(
      get_timeseries_tsid(ts_id, from = from, to = to, datasource = datasource),
      error = function(e) {
        message("⚠️ Skipping ", from, "–", to, " (", e$message, ")")
        NULL
      }
    )
  })

  bind_rows(dfs)
}

# Compute climatology and anomalies
calc_anomaly <- function(full_df, anomaly_start, anomaly_end, res) {
  # ---- Climatology from full dataset ----
  # climatology <- full_df %>%
  #   mutate(doy = yday(Timestamp)) %>%
  #   group_by(doy) %>%
  #   summarize(climatology = rollmean(Value, 7*24*4, align = "center", fill = NA), .groups = "drop")
  dat_prep <- full_df %>%
    mutate(doy = yday(Timestamp))
  roll_mean <- rollmean(dat_prep$Value, 7*24*(60/res), align = "center", fill = NA)#REKENING HOUDEN MET DE RESOLUTIE VAN DE DATA
    
  climatology <- dat_prep %>%  mutate(rollmean = roll_mean) %>%
    group_by(doy) %>%
    summarize(climatology = mean(rollmean, na.rm = TRUE), .groups = "drop")

  # ---- Restrict to anomaly period ----
  df_an <- full_df %>%
    filter(Timestamp >= anomaly_start, Timestamp <= anomaly_end) %>%
    mutate(doy = yday(Timestamp)) %>%
    left_join(climatology, by = "doy") %>%
    mutate(Value = Value - climatology)

  return(df_an)
}
