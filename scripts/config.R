# packages -------------------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
# figure settings -------------------------------------------------------------------------
#style
style <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(size = 30, colour = "black", angle = 90),
  axis.title.x = element_text(size = 30),
  axis.text.y = element_text(size = 30, colour = "black"),
  axis.title.y = element_text(size = 30),
  strip.text = element_text(size = 24), #title of facet wrap bigger
  legend.text = element_text(size = 27),
  legend.title = element_text(size = 30),
  panel.background = element_blank(),
  panel.grid.major = element_line(colour = "grey90")
)
#colours
grey2 <- "#505050"
grey1 <- "#979797"
grey3 <- "#d4d4d4"


