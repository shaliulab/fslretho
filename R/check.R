#' Verify the fslretho installation: configuration readable, folders writable, dependencies present
#' @noRd
check_installation <- function() {
  conf <- tryCatch(fslretho_conf(), error = function(e) {
    stop("The configuration cannot be loaded. Do I have read/write access to /etc/scopr.conf and /etc/fslretho.conf? ",
         conditionMessage(e))
  })

  folders <- c(
    cache = conf$scopr$folders$cache$path,
    sessions = conf$folders$sessions$path,
    logs = conf$folders$logs$path
  )
  for (name in names(folders)) {
    if (file.access(folders[[name]], mode = 2) != 0) {
      stop(sprintf("The %s folder %s is not writable. Please make sure it exists and is writable", name, folders[[name]]))
    }
  }

  if (!file.exists(conf$binaries$python)) stop("Cannot find the python binary under ", conf$binaries$python)
  if (!file.exists(conf$dependencies$ethoscope_imager)) {
    warning("Cannot find ethoscope_imager under ", conf$dependencies$ethoscope_imager)
  }
  invisible(TRUE)
}
