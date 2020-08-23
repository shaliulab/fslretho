#' Start an FSLRetho instance
#'
#' @param display_mode Whether to show the code alongside the application or not
#' @param port Port on which fslretho2 should run
#' @importFrom reactlog reactlog_enable
#' @import shiny
#' @export
main <- function(display_mode=FALSE, port=NULL) {
  # launch the app in a separate browser window
  options(shiny.launch.browser = FALSE)


  # always on the same port: 3838
  if (is.null(port)) {
      options(shiny.port = FSLRethoConfiguration$new()$content[["port"]])
  } else {
      options(shiny.port = port)
  }

  # use the 0.0.0.0 host so the app can be accessed remotely
  options(shiny.host = "0.0.0.0")

  # get an interactive prompt that displays the traceback
  # gives you the ability to interactively debug inside any of the frames
  # options(error = recover)

  options(display.mode = display_mode)

  # Provide a browsable tree of reactive expressions, inputs, and outputs (reactlog)
  reactlog::reactlog_enable()
  options(shiny.fullstacktrace = TRUE)

  shiny::addResourcePath(prefix = "fslretho", directoryPath = system.file(package = "fslretho", "www"))

  # launch the app
  shiny::shinyApp(server = server, ui = ui())
}
