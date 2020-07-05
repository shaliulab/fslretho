# loadDataUI <- function(id) {
#
#   ns <- NS(id)
#
#   tagList(
#     loadEthoscopeUI(ns("loadEthoscope")),
#     loadDamUI(ns("loadDam"))
#   )
#
# }

#' @importFrom shiny tagList NS
#' @importFrom shiny fileInput textInput
loadDataUI <- function(id, monitor, help_text = "") {

  ns <- NS(id)

    # "demonstrating the title case")
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

# loadData <- function(input, output, session) {
#   ethoscope_raw <- callModule(modules[["ethoscope"]][["loader"]], "loadEthoscope")
#   dam_raw <- callModule(modules[["dam"]][["loader"]], "loadDam")
#
#   return(list("ethoscope" = ethoscope_raw, "dam" = dam_raw))
# }

downloadDataUI <- function(id) {

  ns <- NS(id)

}
downloadData <- function(input, output, session) {

}

# ethoscope_data_scored <- callModule(modules[["ethoscope"]][["scorer"]], "scoreEthoscope", ethoscope_data_raw)
# dam_data_scored <- callModule(modules[["dam"]][["scorer"]], "scoreDam", dam_data_raw)