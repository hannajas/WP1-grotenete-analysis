library("remotes")
library("actel")
library(tidyverse)
library(dplyr)

#Data
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)

# max time mag heel hoog --> we willen niet dat hij een nieuwe track begint
# run "runRSP" 
# input = output of residency, migration of explore (actel package)
runSRP(coord.x=deploy_longitude, coord.y=deploy_latitude)

