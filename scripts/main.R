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
source("./data_preprocessing/switch_2D_1D.R")


source("./data_preprocessing/get_cross_section_velocities.R")