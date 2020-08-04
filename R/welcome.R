#' UI for the FSLRetho welcome page
#' @importFrom shiny fluidRow p
welcomePageUI <- function() {

  documentation_url <- "https://docs.google.com/document/d/1cClaDaiQHbexiEVFpVo7bN-09ZgYSw-WLSYToWYuZU4"

  shiny::div(
    shiny::p("Welcome to FSLRetho, an R Shiny web application designed to make Rethomics easy."),
    shiny::p("A comprehensive documentation"),
    shiny::a(href=documentation_url, target="_blank", "is available")
  )
}