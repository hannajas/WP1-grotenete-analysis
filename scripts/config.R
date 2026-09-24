# General packages and figure settings
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

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

# resolution for data interpolation (13_smoothing_interpolation.R and 14_link_env_variables_inter.R)
resolution_s <- "15 min"
minutes <- 15

# Choose datatype (raw or interpolated)
# 03_onset_migration.R: resident vs migratory (raw) OR short term triggers (interpolated)
# 04_during_migration.R: GLMM and general conditions (raw) OR some visualization (interpolated)
data_type <- "raw" # "raw" or "interpolated"
