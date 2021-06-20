conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug

#' Given a metadata file, infer what type of monitor it is
#'
#' @importFrom data.table fread
#' @param metadata_path Path to a metadata file
#' @export
#' @return character either dam or ethoscope
get_monitor_name <- function(metadata_path) {

  metadata <- read.table(metadata_path, comment.char = "#", header = TRUE, sep = ",")
  metadata <- data.table::as.data.table(metadata)

  if(all(c("machine_name", "date") %in% colnames(metadata))) return("ethoscope")

  if(all(c("file", "start_datetime", "stop_datetime") %in% colnames(metadata))) return("dam")
}

# TODO Put in their packages

#' Load a DAM metadata .csv into R
#'
#' @import data.table
#' @importFrom utils read.table
#' @inheritParams get_monitor_name
#' @export
read_dam_metadata <- function(metadata_path) {

  metadata <- read.table(metadata_path, comment.char = "#", header = TRUE, sep = ",")
  metadata <- data.table::as.data.table(metadata)
  return(metadata)
}

#' Load an ethoscope metadata .csv into R and perform basic validation
#' @import data.table
#' @importFrom utils read.table
#' @inheritParams get_monitor_name
#' @export
read_metadata <- function(metadata_path) {
  metadata <- read.table(metadata_path, comment.char = "#", header = TRUE, sep = ",")
  metadata <- data.table::as.data.table(metadata)
  return(metadata)
}


#' Read and validate provided metadata
#' @note `load_metadata` will NOT link the metadata to a local database
#' @seealso https://github.com/rethomics/scopr/blob/master/R/link-ethoscope-metadata.R
#' @seealso https://github.com/rethomics/damr/blob/master/R/link-dam-metadata.R
#' @importFrom magrittr `%>%`
#' @param metadata_path Absolute path to a metadata.csv file
#' @param monitor Name of the monitor that generated the data the passed metadata is trying to load
#' This information is used to select the right validation function
load_metadata <- function(metadata_path, monitor) {
  # Load into R the metadata table

  read_functions <- list("ethoscope" = read_metadata, "dam" = read_dam_metadata)

  if(!monitor %in% names(read_functions)) stop(sprintf("Please enter a valid monitor: currently supported are ethoscope and dam"))

  read_function <- read_functions[[monitor]]

  metadata <- tryCatch({
    # TODO Do I want to handle multiple metadatas?
    if (DEBUG) message("Running read_metadata")
    metadata_list <- lapply(metadata_path, read_function)
    metadata_list %>% lapply(., function(x) x[, colnames(metadata_list[[1]]), with=F]) %>% do.call(rbind, .)
    }, error = function(e) {
      warning(e)
      stop(paste0("Problem combining ", monitor, " metadatas!"))
    }
  )
  return(metadata)
}

validate_metadata <- function(metadata, monitor) {
  validation_functions <- list("ethoscope" = scopr::validate_metadata, "dam" = damr::validate_metadata)
  validation_function <- validation_functions[[monitor]]
  validation_function(metadata)
}

load_metadata_with_handlers <- function(session, ..., monitor) {
  metadata <- withCallingHandlers(
    expr = tryCatch({
      metadata <- load_metadata(..., monitor=monitor)
      showNotification(paste0("Validating ", monitor, " metadata. Please wait..."))
      validate_metadata(metadata, monitor)
      metadata
    }, error = function(e) {
      show_condition_message(e, "error", session)
      list(plot = NULL, data = NULL, layout = NULL)
      shiny::validate(shiny::need(expr = F, label = "metadata is not valid"))
    }
    ),
    warning = function(w) {
      show_condition_message(w, "warning", session)
      list(plot = NULL, data = NULL, layout = NULL)
    }
  )
  return(metadata)
}



loadMetadataServer <- function(id, metadata_path, monitor, result_dir) {

  moduleServer(
    id,
    function(input, output, session) {

      datapath <- reactive({
        metadata_path()[sapply(metadata_path(), get_monitor_name) == monitor]
      })


      metadata <- reactive({
        load_metadata_with_handlers(session, datapath(), monitor = monitor)
      })


      metadata_linked <- reactive({
        scopr::link_ethoscope_metadata(x = metadata(), result_dir = result_dir)
      })


      metadata_link_validated <- reactive({

        if (nrow(metadata_linked()) == 0) {
          shiny::showNotification("Failure: no matches were found in the local ethoscope database.
                           This could be due to typos in the machine_name, date, etc; or
                           your dataset being missing in the database.
                           Check your metadata and/or the local database to find out which is the problem", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        } else {
          showNotification("Success")
        }
      })

      return(metadata_linked)
    }
  )
}


#' Display a data table output of the uploaded metadata
#' It can be filtered by column values and sorted
#' @import shiny
#' @importFrom behavr meta
viewMetadataUI <- function(id) {

  ns <- NS(id)
  dataTableOutput(ns("metadata"))
}

# Generate a data table of the metadata for display in the UI
viewMetadataServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({
        behavr::meta(input_rv$data)
      })

      output$metadata <- renderDataTable({
        metadata()
      })
    }
  )
}
