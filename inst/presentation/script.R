library(data.table)

metadata_files <- c("metadata_ZT20fish_1811.csv", "metadata_ZT8fish_1811.csv")
zts <- c(17, 5)


dts <- lapply(metadata_files, fread)
dt <- dts[[1]]
dt[, zt := zts[1]]
