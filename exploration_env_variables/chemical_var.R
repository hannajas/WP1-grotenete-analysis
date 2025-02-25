
#salinity
# Load metadata
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_S <- filter(metadata, metadata$type == "S")
n <- dim(metadata_S)[1]

for (i in 1:n) {
    path <- paste('./data/raw/salinity/',metadata_S$name[i],'_S.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_S$name[i],'_S', sep =""), temp)
}

# PRE_PROCESSING
for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_S$name[i],'_S.csv', sep ="")
    write.csv(get(paste(metadata_S$name[i],'_S', sep ="")), path)
}


# turbidity
metadata_turb <- filter(metadata, metadata$type == "turb")
n <- dim(metadata_turb)[1]

for (i in 1:n) {
    path <- paste('./data/raw/turbidity/',metadata_turb$name[i],'_turb.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_turb$name[i],'_turb', sep =""), temp)
}

# PRE_PROCESSING
for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_turb$name[i],'_turb.csv', sep ="")
    write.csv(get(paste(metadata_turb$name[i],'_turb', sep ="")), path)
}






# dissolved oxygen
metadata_O <- filter(metadata, metadata$type == "O")
n <- dim(metadata_O)[1]

for (i in 1:n) {
    path <- paste('./data/raw/oxygen/',metadata_O$name[i],'_O.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_O$name[i],'_O', sep =""), temp)
}

# PRE_PROCESSING
for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_O$name[i],'_O.csv', sep ="")
    write.csv(get(paste(metadata_O$name[i],'_O', sep ="")), path)
}