#' @importFrom data.table fread
get_monitor_name <- function(metadata_datapath) {

  metadata <- data.table::fread(cmd = paste0("grep -v '^#' ", metadata_datapath))
  if(all(c("machine_name", "date") %in% colnames(metadata))) return("ethoscope")
  if(all(c("file", "start_datetime", "stop_datetime") %in% colnames(metadata))) return("dam")
}

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

#' Load a DAN metadata .csv into R
#' @importFrom data.table fread
read_dam_metadata <- function(metadata_path) {

  metadata <- data.table::fread(cmd=paste0("grep -v '^#' ", metadata_path))
  metadata$start_datetime <- as.character(metadata$start_datetime)
  metadata$stop_datetime <- as.character(metadata$stop_datetime)
  return(metadata)
}

#' Load an ethoscope metadata .csv into R and perform basic validation
#' @importFrom data.table fread
#' @importFrom magrittr `%>%`
#' @export
read_metadata <- function(metadata_path) {

  metadata <- tryCatch(
    data.table::fread(cmd = paste0("grep -v '^#' ", metadata_path)),
    error = function(e) {
      message(e)
      stop_bad_argument(what = "metadata_path", "cannot be read with fread(). Check it is not malformed. For instance, make sure all rows have same number of columns i.e. same number of commas")
    })

  # change the column zt0 to reference_hour if available
  if ((!"reference_hour" %in% colnames(metadata)) & ("ZT0" %in% colnames(metadata))) {
    colnames(metadata) <- colnames(metadata) %>% gsub(
      pattern = "ZT0",
      x = colnames(metadata),
      replacement =  "reference_hour"
    )
  }

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
load_metadata <- function(metadata_datapath, monitor) {
  # Load into R the metadata table

  if (monitor == "ethoscope") {
    metadata <- tryCatch({
        metadata_list <- lapply(metadata_datapath, read_metadata)
        # browser()
        metadata_list %>% lapply(., function(x) x[, colnames(metadata_list[[1]]), with=F]) %>% do.call(rbind, .)
      }, error = function(e) {
        print(e)
        stop("Problem combining ethoscope metadatas!")
      }
    )
  } else if(monitor == "dam") {
    metadata <- tryCatch({
      metadata_list <- lapply(metadata_datapath, read_dam_metadata)
      # browser()
      metadata_list %>% lapply(., function(x) x[, colnames(metadata_list[[1]]), with=F]) %>% do.call(rbind, .)
    }, error = function(e) {
      print(e)
      stop("Problem combining dam metadatas!")
    })
  } else {
    stop("Monitor not valid. Please pass ethoscope or dam")
  }

  showNotification(glue::glue("Validating {monitor} metadata. Please wait..."))
  # browser()
  if (monitor == "ethoscope") fslscopr::validate_metadata(metadata)
  else if (monitor == "dam") fsldamr::validate_metadata(metadata)
  else stop(sprintf("Please enter a valid monitor: currently supported are ethoscope and dam"))

  # coerce dates and datetimes to character
  char_columns <- c("start_datetime", "stop_datetime", "date")
  for (column_name in char_columns) {
    metadata <- as_character_column(metadata, column_name)
  }

  return(metadata)

}

#' Display a data table output of the uploaded metadata
#' It can be filtered by column values and sorted
#' @import shiny
viewMetadataUI <- function(id) {

  ns <- shiny::NS(id)
  shiny::dataTableOutput(ns("metadata"))
}

# Generate a data table of the metadata for display in the UI
viewMetadataServer <- function(id, rv) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      metadata <- shiny::reactive({
        rv$data[, meta = T]
      })

      output$metadata <- shiny::renderDataTable({
        metadata()
      })
    }
  )
}
