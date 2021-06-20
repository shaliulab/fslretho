
#' Export a database of multiple files into a single .zipo
#'
#' @param dbfiles character, absolute paths to files to be shipped into a single .zip
#' @return path to created zip file in a temporary folder
zip_database <- function(zip_file, files) {

  message(paste0(length(files), " unique dbfiles"))
  message("Creating zip file...")
  zip(zip_file, files)
}