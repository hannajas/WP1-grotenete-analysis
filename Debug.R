test <- align_resolutions_function(
  "Tw",
  as.difftime(5, units = "mins"),#as.period(5, "mins"),
  metadata,
  upsample_method = "linear"
)