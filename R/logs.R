#' Log the user's activity in the logs folder, if shinylogs is installed
#' @noRd
track_usage <- function() {
  if (requireNamespace("shinylogs", quietly = TRUE)) {
    shinylogs::track_usage(storage_mode = shinylogs::store_json(path = fslretho_conf()$folders$logs$path))
  }
}
