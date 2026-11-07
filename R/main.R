#' Start an FSLRetho instance
#'
#' @param port Port on which fslretho should run. Defaults to the port in /etc/fslretho.conf
#' @import shiny
#' @export
main <- function(port = NULL) {
  check_installation()

  options(
    shiny.launch.browser = FALSE,
    shiny.port = if (is.null(port)) fslretho_conf()$port else port,
    # listen on all interfaces so the app can be accessed remotely
    shiny.host = "0.0.0.0",
    shiny.fullstacktrace = TRUE
  )

  # Background R processes for the long tasks (loading data, raw data, snapshots), so that
  # several users can work at the same time without waiting for each other
  if (requireNamespace("future", quietly = TRUE)) {
    workers <- fslretho_conf()$async_workers
    future::plan(future::multisession, workers = if (is.null(workers)) 4 else workers)
  }

  shiny::addResourcePath(prefix = "fslretho", directoryPath = system.file(package = "fslretho", "www"))
  shiny::shinyApp(server = server, ui = ui())
}
