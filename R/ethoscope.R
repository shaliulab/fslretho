conf <- FSLRethoConfiguration$new()
NCORES <- conf$content$scopr$ncores
CACHE <- conf$content$scopr$folders$cache$path
VERBOSE <- TRUE
TESTING <- conf$content$testing
DEBUG <- conf$content$debug

progressBarServer <- function(id, metadata) {

  moduleServer(
    id,
    function(input, output, server) {

      updateProgress <- reactive({

        # disable shiny functionality while testing
        # not ideal but I think tests dont work if I dont disable this
        if (! TESTING) {
          progress <- shiny::Progress$new()
          on.exit(progress$close())

          progress$set(message = "", value = 0)
          n <- nrow(metadata())

          function(detail = NULL) {
            if (NCORES == 1) {
              progress$inc(amount = 1 / n, detail = detail)
            } else {
              shiny::showNotification(detail, type = "message", duration = 2)
            }
          }
        } else {
          function(detail = NULL) {
            message(detail)
          }
        }
      })

      return(updateProgress)

    }
  )
}


loadDtServer <- function(id, metadata, updateProgress_load, updateProgress_annotate) {
  moduleServer(
    id,
    function(input, output, session) {
      dt_raw <- reactive({

        if (DEBUG) message("Running ethoscope data load")
        dt_raw <- scopr::load_ethoscope(
          metadata = metadata(),
          reference_hour = NA,
          ncores = NCORES,
          cache = CACHE,
          verbose = VERBOSE,
          updateProgress_load = updateProgress_load()
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
      updateProgress_load <- progressBarServer("ethoscope-load", metadata)
      updateProgress_annotate <- progressBarServer("ethoscope-annotate", metadata)
      dt_raw <- loadDtServer(
        "dt_raw-ethoscope", metadata,
        updateProgress_load=updateProgress_load, updateProgress_annotate=updateProgress_annotate
      )

      observeEvent(c(submit(), reload()), {

        if (DEBUG) message("Reload or submit detected")
        # req(submit() + reload() > last_reaction)
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
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      return(output_rv)
    }
  )
}

