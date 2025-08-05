plot_one_traj <- function(mydfnew.temp, variable) {
  #n_seg <- length(ltraj_with_segments)
  #data_onetraj$segment <- NA
  #for(i in 1:n_seg){
  #  date <- ltraj_with_segments[[i]]["date"]
  #  data_onetraj$segment[data_onetraj$middledate %in% date$date] <- i
  #}
  if (!("date" %in% colnames(mydfnew.temp))) {
    mydfnew.temp$date <- mydfnew.temp$arrival
  }
  thema <- theme(
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14)
  ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  g <- ggplot() +
    thema +
    theme(axis.text.x = element_blank()) +
    geom_point(
      aes(date, -1 * distance_to_source_m, colour = cluster),
      data = mydfnew.temp,
      shape = 16,
      size = 3
    ) +
    scale_color_manual(values = c("1" = "red", "2" = "green")) +
    #g <- g + geom_point(aes(arrival, photoperiod, colour = "gray"), data = mydfnew.temp, shape = 16, size = 5)
    theme(
      plot.title = element_text(lineheight = .8, face = "bold", size = 20)
    ) +
    #g <- g + scale_y_continuous(limit = c(-240000, 15000),breaks = c(-240000, -220000, -200000, -180000, -160000, -140000,-135000,-130000,-125000,-120000,-115000,-110000,-105000,-100000,-95000,-90000,-85000, -80000, -75000, -70000, -65000, -60000, -55000, -50000, -45000, -40000, -35000, -30000, -25000, -20000, -15000, -10000, -5000, 0, 5000, 10000, 15000), labels = c(-240, -220, -200, -180, -160, -140,-135,-130,-125,-120,-115,-110,-105,-100,-95,-90,-85,-80,-75,-70,-65,-60,-55,-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15))
    labs(
      title = mydfnew.temp$tag_serial_number[1]
    ) +
    ylab("Distance (m)") +
    xlab("Date") + #no -axis.text.x
    # g <- g + scale_x_date(date_labels = "%b-%d-%Y")
    scale_x_datetime(date_breaks = "1 week") +
    annotate(
      "text",
      x = mydfnew.temp$arrival[1],
      y = -1 * mydfnew.temp$distance_to_source_m,
      label = mydfnew.temp$station_name,
      hjust = 0,
      colour = "red",
      size = 3
    ) +
    theme(legend.position = "bottom")
  g2 <- ggplot(mydfnew.temp, aes(date, .data[[variable]])) +
    thema +
    geom_point() +
    geom_line(linewidth = 0.5, linetype = "dashed") +
    labs(x = "Date", y = variable) +
    scale_x_datetime(date_breaks = "1 week")
  return(list(g = g, g2 = g2))
}
