conf <- FSLRethoConfiguration$new()
NCORES <- conf$content$scopr$ncores
CACHE <- conf$content$scopr$folders$cache$path
VERBOSE <- TRUE
TESTING <- conf$content$testing
DEBUG <- conf$content$debug

loadDtServer <- function(id, metadata) {
  moduleServer(
    id,
    function(input, output, session) {
      dt_raw <- reactive({

        progress_bar <- get_progress_bar(nrow(metadata()), "Loading data to memory (R)", ncores=NCORES)
        on.exit(progress_bar$progress$close())


        if (DEBUG) message("Running ethoscope data load")
        dt_raw <- scopr::load_ethoscope(
          metadata = metadata(),
          reference_hour = NA,
          ncores = NCORES,
          cache = CACHE,
          verbose = VERBOSE,
          updateProgress_load = progress_bar$update
        )
        # needed to be able to save the dt
        # because the column file_info is a list
        dt_raw <- fortify(dt_raw, meta = TRUE)

        attr(dt_raw, "monitor") <- "ethoscope"
        dt_raw
      })

      dt_raw_validated <- reactive({
        if (nrow(dt_raw()) == 0) {
          showNotification("Failure: your metadata could be linked but the resulting table is empty", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        }
      })

      return(dt_raw)
    })
}

#' Wrap the ethoscope loading functionality for a ShinyUI
#' @importFrom magrittr `%>%`
#' @importFrom scopr link_ethoscope_metadata load_ethoscope
#' @import shiny
#' @noRd
loadEthoscopeServer <- function(id, metadata_datapath, submit, reload, result_dir) {

  last_reaction <- 0
  debug <- FALSE

  moduleServer(
    id,
    function(input, output, session) {
      output_rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      message("Loading metadata")
      metadata <- loadMetadataServer("metadata-ethoscope", metadata_datapath, "ethoscope", result_dir)

      message("Loading dt_raw")
      dt_raw <- loadDtServer(
        "dt_raw-ethoscope", metadata
      )

      observeEvent(c(submit(), reload()), {

        if (DEBUG) message("Reload or submit detected")
        if((submit() + reload()) > last_reaction) {
          output_rv$data <- dt_raw()
          output_rv$name <- basename(metadata_datapath())
          # signal the reactiveValue is updated to downstream observeEvent blocks
          output_rv$time <- as.numeric(Sys.time())
          last_reaction <<- last_reaction+1
        } else {
          output_rv$data <- NULL
          output_rv$name <- NULL
          output_rv$time <- NULL
        }
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}

