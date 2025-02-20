# Load the environmental data
metadata <- read_csv('./data/raw/metadata/Metadata.csv', show_col_types = FALSE)
metadata_Q <- filter(metadata, metadata$type == "Q")
n <- dim(metadata_Q)[1]

for (i in 1:n) {
    path <- paste('./data/raw/discharge/',metadata_Q$name[i],'_Q.csv', sep ="")
    temp <- read_csv(path)
    assign(paste(metadata_Q$name[i],'_Q', sep =""), temp)
}

# PRE_PROCESSING
rup00a_1066_Q$Timestamp <- floor_date(dmy_hms(rup00a_1066_Q$Timestamp,truncated=3),"day")
zes00a_1066_Q$Timestamp <- floor_date(ymd_hms(zes00a_1066_Q$Timestamp,truncated=3),"day")
zes29f_1066_Q$Timestamp <- floor_date(ymd_hms(zes29f_1066_Q$Timestamp,truncated=3),"day")
for (i in 1:n) {
    path <- paste('./data/interim/processed/',metadata_Q$name[i],'_Q.csv', sep ="")
    write.csv(get(paste(metadata_Q$name[i],'_Q', sep ="")), path)
}