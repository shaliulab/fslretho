conf <- FSLRethoConfiguration$new()
NCORES <- conf$content$scopr$ncores
CACHE <- conf$content$scopr$folders$cache$path
VERBOSE <- TRUE
TESTING <- conf$content$testing
DEBUG <- conf$content$debug


loadDtServer <- function(id, metadata, name=NULL, buttons=reactive(c()), annotation_conf=reactive()) {
  moduleServer(
    id,
    function(input, output, session) {


      output_rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      FUN <- reactive({
        annotation_conf$FUN
      })


      dt <- eventReactive(c(buttons(), reactiveValuesToList(annotation_conf)), {

        req(name())
        progress_bar <- get_progress_bar(nrow(metadata()), "Loading data to memory (R)", ncores=NCORES)
        on.exit(progress_bar$progress$close())

        annotation_params <- reactiveValuesToList(annotation_conf)
        params <- annotation_params[setdiff(names(annotation_params), "intervals")]
        intervals <- annotation_params$intervals


        if (DEBUG) message("Running ethoscope data load")

        dt <- do.call(
          scopr::load_ethoscope,
          append(list(
            metadata = metadata(),
            reference_hour = NA,
            ncores = NCORES,
            cache = CACHE,
            verbose = VERBOSE,
            callback = progress_bar$update,
            intervals = intervals,
            columns_to_keep =  c(
              "t", "x", "y", "max_velocity", "body_movement", "micromovement",  "microbehavior", "interactions",
              "beam_crosses", "moving","asleep", "is_interpolated")

          ),
          params
          )
        )

        dt <- integrate_annotations(dt)

        # needed to be able to save the dt
        # because the column file_info is a list
        dt <- behavr::simplify_behavr(dt, meta = TRUE)

        #qc <- load_ethoscope_qc(
        #  metadata = metadata()
        #)

        attr(dt, "monitor") <- "ethoscope"
        dt
      }, ignoreInit = TRUE)

      dt_validated <- reactive({
        if (nrow(dt()) == 0) {
          showNotification("Failure: your metadata could be linked but the resulting table is empty", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        }
      })


      observeEvent(dt(), {
        req(dt())
        output_rv$data <- dt()
        output_rv$name <- name()
        output_rv$time <- as.numeric(Sys.time())
      })

      return(output_rv)
    })
}

#' Wrap the ethoscope loading functionality for a ShinyUI
#' @importFrom magrittr `%>%`
#' @importFrom scopr link_ethoscope_metadata load_ethoscope
#' @param metadata_datapath Path to a .csv with the user passed metadata
#' @param submit reactive, triggered manually by the user to submit a metadata file to the application
#' @param reload, reactive, triggered manually by the user to reload a dataset
#' @param result_dir slot of a reactiveValues object containing the path to an ethoscope database
#' @param Additional parameters to loadDtServer
#' @noRd
loadEthoscopeServer <- function(id, metadata_datapath, submit, reload, result_dir, ...) {

  debug <- FALSE

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      buttons <- reactive({
        c(submit(), reload())
      })

      message("Loading metadata")
      metadata <- loadMetadataServer("metadata-ethoscope", metadata_datapath, "ethoscope", result_dir)

      message("Loading dt_raw")
      dt_rv <- loadDtServer(
        "dt-ethoscope", metadata, name=reactive(basename(req(metadata_datapath()))), buttons=buttons, ...
      )

      observeEvent(dt_rv$time, {

        req(metadata_datapath())
        req(dt_rv$data)
        req(dt_rv$time)
        if (DEBUG) message("Reload or submit detected")
        output_rv$data <- dt_rv$data
        output_rv$name <- dt_rv$name
        output_rv$time <- dt_rv$time
        # signal the reactiveValue is updated to downstream observeEvent blocks
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}

