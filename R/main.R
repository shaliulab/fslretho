#' Start an FSLRetho instance
#'
#' @export
main <- function() {
  # launch the app in a separate browser window
  options(shiny.launch.browser = FALSE)

  # always on the same port: 3838
  options(shiny.port = FSLRethoConfiguration$new()$content[["port"]])

  # use the 0.0.0.0 host so the app can be accessed remotely
  options(shiny.host = "0.0.0.0")

  # get an interactive prompt that displays the traceback
  # gives you the ability to interactively debug inside any of the frames
  # options(error = recover)

  # launch the app
  shiny::shinyApp(server = server, ui = shinydashboard_ui())
}

