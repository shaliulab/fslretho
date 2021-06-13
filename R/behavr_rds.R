#' @export
saveRDS_behavr <- function(object, file, ...) {

  stopifnot("behavr" %in% class(object))

  file <- gsub(pattern = "RDS", replacement = "rds", x = file)

  main_table_file <- paste0(gsub(x = file, pattern = ".rds", replacement = ""), "_main.rds")
  meta_table_file <- paste0(gsub(x = file, pattern = ".rds", replacement = ""), "_meta.rds")

  saveRDS(object = object, file = main_table_file, ...)
  saveRDS(object = object[, meta=TRUE], file = meta_table_file, ...)
}

#' @export
readRDS_behavr <- function(file, ...) {

  file <- gsub(pattern = "RDS", replacement = "rds", x = file)

  main_table_file <- paste0(gsub(x = file, pattern = ".rds", replacement = ""), "_main.rds")
  meta_table_file <- paste0(gsub(x = file, pattern = ".rds", replacement = ""), "_meta.rds")

  main_table <- readRDS(file = main_table_file, ...)
  meta_table <- readRDS(file = meta_table_file, ...)
  x <- behavr::setmeta(main_table, meta_table)
  return(x)
}