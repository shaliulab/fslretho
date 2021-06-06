

progressBarServer <- function(id, metadata) {

  moduleServer(
    id,
    function(input, output, server) {

      updateProgress <- reactive({

        if (! fslretho::FSLRethoConfiguration$new()$content$testing) {
          progress <- shiny::Progress$new()
          on.exit(progress$close())

          progress$set(message = "", value = 0)
          n <- nrow(metadata())

          function(detail = NULL) {
            if (FSLRethoConfiguration$new()$content[["ncores"]] == 1) {
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

        message("Calling scopr::load_ethoscope")


        dt_raw <- scopr::load_ethoscope(
          metadata = metadata(),
          reference_hour = NA,
          ncores = FSLRethoConfiguration$new()$content[["ncores"]],
          cache = FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]],
          verbose = TRUE,
          updateProgress_load = updateProgress_load(),
          updateProgress_annotate = updateProgress_annotate()
        )
        # needed to be able to save the dt
        # because the column file_info is a list
        dt_raw <- fortify(dt_raw, meta = TRUE)
        message("Data loaded into R successfully")

        attr(dt_raw, "monitor") <- "ethoscope"
        dt_raw
      })

      dt_raw_validated <- reactive({
        if (nrow(dt_raw()) == 0) {
          showNotification("Failure: your metadata could be linked but the resulting table is empty", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        }
      })

      observe(
        dt_raw_validated()
      )

      return(dt_raw)
    })
}

#' Wrap the ethoscope loading functionality for a ShinyUI
#' @importFrom magrittr `%>%`
#' @importFrom scopr link_ethoscope_metadata load_ethoscope
#' @import shiny
#' @noRd
loadEthoscopeServer <- function(id, metadata_datapath, submit, reload, result_dir) {

  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(
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

      observeEvent(c(submit, reload()), {
        message("Reload or submit detected")
        if(isTruthy(metadata())) {
          rv$data <- dt_raw()
          rv$name <- basename(metadata_datapath())
          rv$time <- as.numeric(Sys.time())
        } else {
          rv$data <- NULL
          rv$name <- NULL
          rv$time <- NULL
        }
      }, ignoreInit = TRUE)

      return(rv)
    }
  )
}

