# Load from /raw folder preprocess and write to /processed folder
# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be

# Load metadata
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_R <- filter(metadata, metadata$type == "R")
n <- dim(metadata_R)[1]

for (i in 1:n) {
  path <- paste('./data/raw/rainfall/', metadata_R$name[i], '_R.csv', sep = "")
  temp <- read_csv(path)
  assign(paste(metadata_R$name[i], '_R', sep = ""), temp)
}

# PRE_PROCESSING
for (i in 1:n) {
  path <- paste(
    './data/interim/processed/',
    metadata_R$name[i],
    '_R.csv',
    sep = ""
  )
  write.csv(get(paste(metadata_R$name[i], '_R', sep = "")), path)
}
