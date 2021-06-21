track_usage <- function() {

  # Log relevant events made by the user
  if (requireNamespace("shinylogs", quietly = TRUE)) {
    conf <- FSLRethoConfiguration$new()
    logs_folder <- conf$content$folders$logs$path
    shinylogs::track_usage(storage_mode = shinylogs::store_json(path = logs_folder))
  }
}
