#' Wrapper around the most common workflow to laod ethoscope data into R using Rethomics
#'
#' @param metadata A valid data.table object describing an experimental design
#' @param result_dir Absolute path of an ethoscope sqlite3 database
#' @param reference_hour hour of the day when the light turns on in the experiment
#' e.g. if the lights turn on at 8 AM GMT, then reference_hour = 8
#' If NULL, the reference_hour is parsed from the column with the same name in the metadata
#' If such a column is not available, then assume ZT is the same as the experiment start
#' @param updateProgress Optional, a function that updates an object of class shiny::Progress
#'
#' @return A raw behavr table i.e. without annotation
load_ethoscope <- function(metadata, result_dir, reference_hour = NULL, updateProgress = NULL) {

  # Link metadata to the ethoscope database i.e.
  # add to each fly pointer to the dbfile in the database where it is stored
  # This pointer is exploited in the fslscopr::load_ethoscope function
  metadata <- fslscopr::link_ethoscope_metadata(x = metadata, result_dir = result_dir)

  # Load into R the behavioral dataset described in the user passed metadata
  dt <- fslscopr::load_ethoscope(
    metadata = metadata,
    reference_hour = reference_hour,
    ncores = FSLRethoConfiguration$new()$content[["ncores"]],
    cache = FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]],
    verbose = FSLRethoConfiguration$new()$content[["debug"]],
    updateProgress = updateProgress
  )
  return(dt)
}

#' Wrap the ethoscope loading functionality for a ShinyUI
#' @noRd
loadEthoscopeServer <- function(id, last_monitor, dataset_name) {
  moduleServer(
    id,
    function(input, output, session) {


      metadata <- reactive({
        load_metadata(input$metadata$datapath, monitor = "ethoscope")
      })

      dt_raw <- reactive({

        message("Loading ethoscope data")

        # TODO Can this all be packaged into a function?
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "Loading ROI number ", value = 0)
        n <- nrow(metadata())

        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1 / n, detail = detail)
        }
        ##

        dt_raw <- load_ethoscope(metadata(), result_dir = input$result_dir, updateProgress = updateProgress)
        return(dt_raw)

      })

      # make it eager
      observeEvent(input$submit, {
        print(dt_raw())
        last_monitor("ethoscope")
        dataset_name(input$metadata$name)
      })

      return(dt_raw)
    }
  )
}