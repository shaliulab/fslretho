#' Define groups of animals relevant to the user
#'
#' @importFrom shiny NS uiOutput
defineGroupUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::uiOutput(ns("animal_groups"))
}

defineGroupServer <- function(dt) {
  moduleServer(
    id,
    function(input, output, session) {

      output$animal_groups <- renderUI({


      })
    }
  )
}