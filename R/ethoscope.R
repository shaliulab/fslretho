#' Wrapper around the most common workflow to laod ethoscope data into R using Rethomics
#'
#' @importFrom magrittr `%>%`
#' @importFrom fslscopr link_ethoscope_metadata load_ethoscope
#' @param metadata A valid data.table object describing an experimental design
#' @param result_dir Absolute path of an ethoscope sqlite3 database
#' @param reference_hour hour of the day when the light turns on in the experiment
#' e.g. if the lights turn on at 8 AM GMT, then reference_hour = 8
#' If NULL, the reference_hour is parsed from the column with the same name in the metadata
#' If such a column is not available, then assume ZT is the same as the experiment start
#' @param updateProgress Optional, a function that updates an object of class shiny::Progress
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
#' @importFrom magrittr `%>%`
#' @importFrom fslscopr link_ethoscope_metadata load_ethoscope
#' @noRd
loadEthoscopeServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


      rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      metadata <- reactive({

      withCallingHandlers(
          expr = tryCatch({
            load_metadata(input$metadata$datapath, monitor = "ethoscope")
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

      })

      metadata_linked <- reactive({
        fslscopr::link_ethoscope_metadata(x = metadata(), result_dir = input$result_dir)
      })


      dt_raw <- reactive({

        if (nrow(metadata_linked()) == 0) {
          showNotification("Failure: no matches were found in the local ethoscope database.
                           This could be due to typos in the machine_name, date, etc; or
                           your dataset being missing in the database.
                           Check your metadata and/or the local database to find out which is the problem", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        } else {
          showNotification("Success")
        }
        message("Loading ethoscope data")

        # TODO Can this all be packaged into a function?
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "Loading ROI number ", value = 0)
        n <- nrow(metadata())

        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1 / n, detail = detail)
        }

        # browser()

        dt_raw <- fslscopr::load_ethoscope(
          metadata = metadata_linked(),
          reference_hour = NULL,
          ncores = FSLRethoConfiguration$new()$content[["ncores"]],
          cache = FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]],
          verbose = TRUE,
          updateProgress = updateProgress
        )  %>%
          fortify(., meta = TRUE)

        attr(dt_raw, "monitor") <- "ethoscope"
        dt_raw
      })

      observeEvent(input$submit, {
        rv$data <- dt_raw()
        rv$name <- input$metadata$name
        rv$time <- as.numeric(Sys.time())

      })

      return(rv)
    }
  )
}