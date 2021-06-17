#' Verify the fslretho installation
check_installation <- function() {

  conf <- tryCatch({
    conf <- FSLRethoConfiguration$new()
  }, error = function(e) {
    stop("Configuration cannot be loaded.
         Do I have read/write access to /etc/scopr.conf and /etc/fslretho.conf?")
  })

  ethoscope_cache <- conf$content$folders$ethoscope_cache$path
  status <- file.access(ethoscope_cache, mode = 2)
  if (status != 0) stop(paste0("Cache directory ", ethoscope_cache, " is not writable.
                               Please make sure it exists and is writable"))

  sessions_folder <- file.path(ethoscope_cache, "sessions")
  if (!dir.exists(sessions_folder))
    tryCatch(
      dir.create(sessions_folder),
      error = function(e) {
        stop(paste0("Cannot create the sessions folder under ", ethoscope_cache))
      }
    )
}