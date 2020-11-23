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
          value = FSLRethoConfiguration$new()$content[["folders"]][["ethoscope"]][["path"]]
        ),
        shiny::textInput(
          inputId = ns("result_dir_dam"), label = "",
          value = FSLRethoConfiguration$new()$content[["folders"]][["dam"]][["path"]]
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

      rv <- reactiveValues(
        ethoscope = NULL,
        dam = NULL
      )

      metadata_datapath <- reactive({
        input$metadata$datapath
      })

      rv <- reactiveValues(
        ethoscope = loadEthoscopeServer("ethoscope", metadata_datapath, reload, input, session),
        dam = loadDamServer("dam", metadata_datapath, reload, input, session)
      )

      observe({
        req(rv$ethoscope)
        req(rv$dam)
      })

      return(rv)
    }
  )
}