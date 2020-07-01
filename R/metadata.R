#' Read and validate provided metadata
#'
#' @importFrom data.table fread
#' @param metadata_path Absolute path to a metadata.csv file
#' @param monitor Name of the monitor that generated the data the passed metadata is trying to load
#' This information is used to select the right validation function
load_metadata <- function(metadata_path, monitor) {
  # Load into R the metadata table
  metadata <- data.table::fread(metadata_path)
  # Validate the user passed metadata
  # If it's fine, it returns TRUE,
  # otherwise, an error is raised and is presented in the UI

  if (monitor == "ethoscope") fslscopr::validate_metadata(metadata)
  else if (monitor == "dam") fsldamr::validate_metadata(metadata)
  else stop(sprintf("Please enter a valid monitor: currently supported are ethoscope and dam"))
  return(metadata)

}


viewMetadataUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::dataTableOutput(ns("metadata"))
}

viewMetadataServer <- function(id, scored_data) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({
        scored_data$data()[, meta = T]
      })

      output$metadata <- shiny::renderDataTable({
        metadata()
      })
    }
  )
}

