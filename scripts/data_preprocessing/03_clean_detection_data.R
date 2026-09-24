# Data cleaning by removing ghost and false detections.
# Ghost and false detections can be checked through the interactive html maps (see 'create_interactive_maps.R')
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Remove detections on irrelevant code networks
unique(data$acoustic_project_code)

data <- data[!(data$acoustic_project_code == "MOBEIA"), ]
data <- data[!(data$acoustic_project_code == "bpns"), ]
data <- data[!(data$acoustic_project_code == "SPAWNSEIS"), ]
data <- data[!(data$acoustic_project_code == "cpodnetwork"), ]
data <- data[!(data$acoustic_project_code == "OP-Test"), ]
data <- data[!(data$acoustic_project_code == "CONNECT-MED"), ]
data <- data[!(data$acoustic_project_code == "Orstedcod"), ]

