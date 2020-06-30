#' UI for the FSLRetho welcome page
#' @importFrom shiny fluidRow p
welcomePageUI <- function() {

  shiny::fluidRow(
    shiny::p("Welcome to FSLRetho, a GUI to the Rethomics.")
  )
}