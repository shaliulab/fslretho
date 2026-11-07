# Usage: Rscript install_dependencies.R [DEPENDENCIES_DIR]   (default: /opt)
# Installs the packages listed in DEPENDENCIES_DIR/unique_dependencies.txt that are missing,
# from the CRAN snapshot in the CRAN_SNAPSHOT environment variable if set
args <- commandArgs(trailingOnly = TRUE)
setwd(if (length(args) > 0) args[1] else "/opt")
repos <- Sys.getenv("CRAN_SNAPSHOT", unset = getOption("repos")[["CRAN"]])
deps <- as.character(read.table("unique_dependencies.txt")$V1)
deps <- setdiff(deps, rownames(installed.packages()))
if (length(deps) > 0) install.packages(deps, repos = repos)
missing <- setdiff(deps, rownames(installed.packages()))
if (length(missing) > 0) stop("Could not install: ", paste(missing, collapse = ", "))
