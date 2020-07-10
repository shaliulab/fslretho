#' Coerce a column to character
#'
#' Make some columns that could be considered numeric character
#' because that way thei handling is easier in esquisse
as_character_column <- function(metadata, column_name) {

  if (column_name %in% colnames(metadata)) {
    metadata[[column_name]] <- as.character(metadata[[column_name]])
  }

  return(metadata)
}


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

  # change the column zt0 to reference_hour if available
  if ((! "reference_hour" %in% colnames(metadata)) & ("ZT0" %in% colnames(metadata))) {
    colnames(metadata) <- colnames(metadata) %>% gsub(
      pattern = "ZT0",
      x = colnames(metadata),
      replacement =  "reference_hour"
    )
  }

  if (monitor == "ethoscope") fslscopr::validate_metadata(metadata)
  else if (monitor == "dam") fsldamr::validate_metadata(metadata)
  else stop(sprintf("Please enter a valid monitor: currently supported are ethoscope and dam"))

  char_columns <- c("start_datetime", "stop_datetime", "date")
  for (column_name in char_columns) {
    metadata <- as_character_column(metadata, column_name)
  }

  return(metadata)

}


viewMetadataUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::dataTableOutput(ns("metadata"))
}

viewMetadataServer <- function(id, grouped_data) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({
        grouped_data()$data()[, meta = T]
      })

      output$metadata <- shiny::renderDataTable({
        metadata()
      })
    }
  )
}

