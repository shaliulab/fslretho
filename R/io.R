#' @import shiny
#' @importFrom tools toTitleCase
loadDataUI <- function(id, help_text = "") {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::mainPanel(
      h2(tools::toTitleCase("Metadata input (DAM/ethoscope)")),
      shiny::fluidRow(
        shiny::fileInput(inputId = ns("metadata"), label = "",
                         multiple = TRUE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"
                         ),
        ),

        shiny::textInput(
          inputId = ns("result_dir_ethoscope"), label = "",
          value = FSLRethoConfiguration$new()$content$scopr$folders$results$path
        ),
        shiny::textInput(
          inputId = ns("result_dir_dam"), label = "",
          value = FSLRethoConfiguration$new()$content$damr$folders$results$path
        ),

        shiny::actionButton(ns("submit"), label = "Submit")
      )
    ),
    shiny::sidebarPanel(p(help_text))
  )
}


loadDataServer <- function(id, reload) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        ethoscope = reactiveValues(data=NULL, name=NULL, time=NULL),
        dam = reactiveValues(data=NULL, name=NULL, time=NULL)
      )

      submit <- reactive(
        input$submit
      )

      metadata_datapath <- reactive({
        input$metadata$datapath
      })


      ethoscope_result <- loadEthoscopeServer("ethoscope", metadata_datapath, submit, reload, input$result_dir_ethoscope)

      observeEvent(ethoscope_result$time, {
        output_rv$ethoscope$data <- ethoscope_result$data
        output_rv$ethoscope$name <- ethoscope_result$name
        output_rv$ethoscope$time <- ethoscope_result$time
      })
      # dam_result <- loadDamServer("dam", metadata_datapath, submit, reload, input$result_dir_dam)

      return(output_rv)
    }
  )
}
