#' Define groups of animals relevant to the user
#'
#' @importFrom shiny NS actionButton tags
conditionalGroupUI <- function(id) {

  ns <- shiny::NS(id)
  tagList(
    actionButton(ns('insertBtn'), 'Insert group'),
    actionButton(ns('removeBtn'), 'Remove group'),
    tags$div(id = ns('placeholder'))
  )
}

conditionalGroupServer <- function(dt) {
  moduleServer(
    id,
    function(input, output, session) {

      inserted <- c()

      observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- paste0('txt', btn)
        insertUI(
          selector = '#placeholder',
          ## wrap element in a div with id for ease of removal
          ui = defineGroupUI("defineGroup")
        )
        inserted <<- c(id, inserted)
      })

      observeEvent(input$removeBtn, {
        removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted[length(inserted)])
        )
        inserted <<- inserted[-length(inserted)]
      })

    }
  )
}

defineGroupUI <- function(id) {

  ns <- shiny::NS(id)
  uiOutput(ns("conditional_animal_group"))
  shiny::verbatimTextOutput(ns("dt_summary"))
  shiny::verbatimTextOutput(ns("metadata_summary"))

}

defineGroupServer <- function(id, dt) {
  moduleServer(
    id,
    function(input, output, session) {

      choices <- colnames(dt())

      output$dt_summary <- renderPrint({
        cat(
          summary(
            dt()
          )
        )
      })

      output$metadata_summary <- renderPrint({
        cat(
          summary(
            dt()[, meta = TRUE]
          )
        )
      })

      output$conditional_animal_group <- renderUI({
        tagList(
          shiny::selectizeInput(session$ns("category"), choices, multiple = FALSE),
          shiny::textInput(session$ns("value"), label = "", value = "")

        )
      })

    }
  )
}

