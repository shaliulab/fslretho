#' @import shiny
#' @importFrom tools toTitleCase
loadDataUI <- function(id, monitor, help_text = "") {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::mainPanel(
      h2(tools::toTitleCase(sprintf("%s input", monitor))),
      shiny::fluidRow(
        shiny::fileInput(inputId = ns("metadata"), label = "",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"
                         ),
        ),

        shiny::textInput(
          inputId = ns("result_dir"), label = "",
          value = FSLRethoConfiguration$new()$content[["folders"]][[monitor]][["path"]]
        ),

        shiny::actionButton(ns("submit"), label = "Submit")
      )
    ),
    shiny::sidebarPanel(p(help_text))
  )
}