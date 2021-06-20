debugModuleServer  <- function(id, envir) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$button, {
        eval(expr = expr(browser()), envir = envir)
      })
    }
  )
}

debugModuleUI <- function(id) {

  ns <- NS(id)
  actionButton(ns("button"), label = "debug", icon = icon("life-ring"))
}