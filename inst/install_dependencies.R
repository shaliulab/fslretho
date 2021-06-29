setwd("/opt")
deps <- as.character(read.table("unique_dependencies.txt")$V1)
lapply(deps, install.packages)

