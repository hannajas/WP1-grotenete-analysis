# --------------------------------------------------------------------------------------------
# IF data not yet available (./data)  --------------------------------------------------------
# UNCOMMENT LINE BELOW TO GET ENVIRONMENTAL DATA FROM WATERINFO
# source("./data_download/wateRinfo.R")

# --------------------------------------------------------------------------------------------
#load all general settings  ------------------------------------------------------------------
source("./config.R")

# --------------------------------------------------------------------------------------------
#load some functions  ------------------------------------------------------------------
source("./src/pdf_trajectory.R")

# --------------------------------------------------------------------------------------------
#data preprocessing  -------------------------------------------------------------------------
source("./data_preprocessing/02_merge_eel_characteristics.R")
source("./data_preprocessing/03_clean_detection_data.R")
source("./data_preprocessing/04_extract_network.R")
source("./data_preprocessing/05_smooth_eel_tracks.R")
source("./data_preprocessing/06_switch_2D_1D.R")
source("./data_preprocessing/07_preprocessing_INBO_data.R")

#source 08_environmental_variables
source("./data_preprocessing/08_environmental_variables/circadian.R")
source("./data_preprocessing/08_environmental_variables/chemical_var.R")
source("./data_preprocessing/08_environmental_variables/discharge.R")
source("./data_preprocessing/08_environmental_variables/watertemperature.R")
source("./data_preprocessing/08_environmental_variables/photoperiod.R")
source("./data_preprocessing/08_environmental_variables/tides.R")
source("./data_preprocessing/08_environmental_variables/velocity.R")
source("./data_preprocessing/08_environmental_variables/rainfall.R")
source("./data_preprocessing/08_environmental_variables/metadata.R")
source("./data_preprocessing/08_environmental_variables/lunar_cycle.R")

source("./data_preprocessing/09_get_cross_section_velocities.R")
source("./data_preprocessing/10_link_env_variables.R")
source("./data_preprocessing/11_smoothing_interpolation.R")
source("./data_preprocessing/12_link_env_variables_inter.R")

# --------------------------------------------------------------------------------------------
#data analysis  -------------------------------------------------------------------------
source("./analysis/01_clustering.R")
source("./analysis/02_onset_migration.R")
source("./analysis/03_duration_migration.R")
