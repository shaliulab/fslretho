#' Verify the fslretho installation
check_installation <- function() {

  scopr_conf <- tryCatch({
    conf <- scopr::scoprConfiguration$new()
  }, error = function(e) {
    stop("scopr configuration cannot be loaded.
         Do I have read/write access to /etc/scopr.conf?")
  })

  conf <- tryCatch({
    conf <- FSLRethoConfiguration$new()
  }, error = function(e) {
    stop("Configuration cannot be loaded.
         Do I have read/write access to /etc/scopr.conf and /etc/fslretho.conf?")
  })

  ethoscope_cache <-  scopr_conf$content$folders$cache$path
  status <- file.access(ethoscope_cache, mode = 2)
  if (status != 0) stop(paste0("Cache directory ", ethoscope_cache, " is not writable.
                               Please make sure it exists and is writable"))

  sessions_folder <- conf$content$folders$ethoscope_sessions$path
  status <- file.access(sessions_folder, mode = 2)
  if (status != 0) stop(paste0("Cache directory ", sessions_folder, " is not writable.
                               Please make sure it exists and is writable"))

  logs_folder  <- conf$content$folders$logs$path
  status <- file.access(sessions_folder, mode = 2)
  if (status != 0) stop(paste0("Logs directory ", logs_folder, " is not writable.
                               Please make sure it exists and is writable"))

}
