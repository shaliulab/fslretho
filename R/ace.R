#' @import shiny
#' @importFrom shinyAce aceEditor
editorUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    shiny::column(width = 4,
                  shinyAce::aceEditor(
                    ns("code"),
                    mode = "r",
                    height = "200px",
                    autoComplete = "live",
                    autoCompleters = "rlang",
                    hotkeys = list(
                      runKey = list(
                        win = "Ctrl-Enter",
                        mac = "CMD-ENTER"
                      )
                    )
                  )
    ), # column
    shiny::column(width = 8,
                  tags$div(
                    style = "overflow-y:scroll; max-height: 300px;",
                    verbatimTextOutput(ns("codeOutput"))
                  )
    ), # column,
    tags$div(
      style = "overflow-y:scroll; max-height: 300px;",
      plotOutput("plotOutput")
    )
  ) # row
}

#' @importFrom shinyAce aceAutocomplete
editorSERVER <- function(
  input,
  output,
  session
) {

  # ns <- session$ns
  shinyAce::aceAutocomplete("code")
  tmpFile <- tempfile()

  result <- reactive({
    val <- input$code_runKey
    if (is.null(val)) {
      return(
        "Execute [R] chunks with Ctrl/Cmd-Enter"
      )
    }

    selectionLocal <- val$selection
    lineLocal <- val$line
    if (val$selection != "") {
      toExecute <- selectionLocal
    } else {
      shiny::validate(
        need(!is.null(lineLocal), "Unable to find execution text")
      )
      toExecute <- lineLocal
    }
    writeLines(toExecute, con = tmpFile)
    source(tmpFile, echo = TRUE, local = TRUE)
  })

  output$codeOutput <- renderPrint({
    result()
  })

  output$plotOutput <- renderPlot({
    result()$value
  })

}
