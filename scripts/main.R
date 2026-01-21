# --------------------------------------------------------------------------------------------
# IF data not yet available (./data)  --------------------------------------------------------

# get environmental variables from wateRinfo
source("./data_download/wateRinfo.R")
#get raw detection data??

# --------------------------------------------------------------------------------------------
#load all general settings  ------------------------------------------------------------------
source("./config.R")

# --------------------------------------------------------------------------------------------
#data preprocessing  -------------------------------------------------------------------------
source("./data_preprocessing/01_switch_2D_1D.R")
source("./data_preprocessing/02_preprocessing_INBO_data.R")
#source 03_environmental_variables
source("./data_preprocessing/03_environmental_variables/circadian.R")
source("./data_preprocessing/03_environmental_variables/chemical_var.R")
source("./data_preprocessing/03_environmental_variables/discharge.R")
source("./data_preprocessing/03_environmental_variables/watertemperature.R")
source("./data_preprocessing/03_environmental_variables/photoperiod.R")
source("./data_preprocessing/03_environmental_variables/tides.R")
source("./data_preprocessing/03_environmental_variables/velocity.R")
source("./data_preprocessing/03_environmental_variables/rainfall.R")
source("./data_preprocessing/03_environmental_variables/metadata.R")
source("./data_preprocessing/03_environmental_variables/lunar_cycle.R")

source("./data_preprocessing/04_get_cross_section_velocities.R")
source("./data_preprocessing/05_link_env_variables.R")
source("./data_preprocessing/06_link_env_variables_inter.R")

# --------------------------------------------------------------------------------------------
#data analysis  -------------------------------------------------------------------------
source("./analysis/01_clustering.R")
source("./analysis/02_onset_migration.R")
source("./analysis/03_duration_migration.R")
