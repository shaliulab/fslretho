FUNCTION_MAP <- list(ethoscope = sleepr::sleep_annotation, dam = sleepr::sleep_dam_annotation)

conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug


#' Produce a Shiny progress bar
#' @param steps Number of steps in the progress bar i.e. the number of times
#' the progress bar needs to be advanced by 1 to have it completed
get_progress_bar <- function(steps) {
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = "Scoring ", value = 0)

  progress_bar <- function(detail = NULL) {

    progress$inc(amount = 1 / steps, detail = detail)
  }
  return(progress_bar)
}

#' @importFrom shiny NS uiOutput
scoreDataUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::sliderInput(ns("velocity_correction_coef"), label = "Threshold (velocity correction coef)", min = 0.001, max = 0.006, value = 0.0048, step = 0.0001),
    shiny::sliderInput(ns("min_time_immobile"), label = "Mimimum time immobile", min = 100, max = 600, value = 300, step = 10),
    shiny::sliderInput(ns("time_window_length"), label = "Window duration", min = 5, max = 60, value = 10, step = 5),
    shiny::selectizeInput(ns("FUN"), label = "", choices = c("sleep_annotation"))
  )
  # shiny::uiOutput(ns("scoringInput"))
}

#' Annotate behavior coming from DAM or ethoscope
#'
#' This function takes dependence on reactiveValues.
#' Thus it needs to be run on a reactive environment (observe/reactive)
#' @importFrom scopr annotate_all
#' @importFrom sleepr sleep_annotation sleep_dam_annotation
#' @param input_rv reactiveValues, incoming dataset with data, name and time slots
#' @param updateProgress function optionally taking a character that executes every time a new individual is processed
#' @param ... Additional arguments to scoring function
#' @export
score_monitor <- function(input_rv, FUN, updateProgress, ...) {

  SPARSE_DATA <- getOption("sparse_data", FALSE)
  CURATE <- ifelse(SPARSE_DATA, FALSE, TRUE)

  output_rv <- reactiveValues(
    data = NULL,
    name = NULL,
    time = NULL
  )

  data <- input_rv$data

  scoring_parameters <- list(...)[[1]]

  if (SPARSE_DATA) {
    message("score module overrides time_window_length to 1 hour")
    scoring_parameters$time_window_length <- hours(1)
  }

  # Annotate all ROIS with all scoring FUN passed by the user
  args <- append(
    scoring_parameters,
    list(
      data = data,
      FUN = FUN,
      curate = CURATE,
      updateProgress = updateProgress
    )
  )

  if (DEBUG) message("Running behavioral annotation")
  data_annotated <- do.call(scopr::annotate_all, args)

  validate(need(nrow(data_annotated) > 0, "Data cannot be annotated. This could be due to your dataset being sparse"))
  output_rv$data <- data_annotated
  output_rv$name <- input_rv$name
  output_rv$time <- input_rv$time
  return(output_rv)
}


#' Shiny module to automatically score a raw behavr table
#'
#' Provide a multi-animal reactive behavr and return the scored version
#'
#' @param id Module id - character
#' @param input_rv reactiveValues with one slot per monitor
#' @param pb logical, if TRUE, a shiny progress bar will track the processing of each individual
#' @importFrom shiny moduleServer reactive observe eventReactive Progress
#' @importFrom behavr bin_apply_all
#' @importFrom rlang fn_fmls
scoreDataServer <- function(id, input_rv, pb=TRUE) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        ethoscope = reactiveValues(data  = NULL, name = NULL, time = NULL),
        dam = reactiveValues(data  = NULL, name = NULL, time = NULL)
      )

      # Get just the annotation parameters
      scoring_parameters <- reactive(
        list(
          velocity_correction_coef = input$velocity_correction_coef,
          min_time_immobile = input$min_time_immobile,
          time_window_length = input$time_window_length
        )
      )

      # remove the parameters that the DAM scoring function does not need
      # it would error otherwise, Error in FUN: unused arguments
      # (velocity_correction_coef = 3e-04, time_window_length = 10, curate = TRUE)

      dam_scoring_parameters <- reactive({
        x <- scoring_parameters()
        x$time_window_length <- NULL
        x$curate <- NULL
        x$velocity_correction_coef <- NULL
        x
      })

      ethoscope_pb <- reactive({
        if (pb) {
          n_individuals <- nrow(input_rv$ethoscope$data[, meta = T])
          get_progress_bar(n_individuals)
        } else {
          NULL
        }
      })


      dam_pb <- reactive({
        if (pb) {
          n_individuals <- nrow(input_rv$dam$data[, meta = T])
          get_progress_bar(n_individuals)
        } else {
          NULL
        }
      })

      # Score ethoscope
      observeEvent(c(input_rv$ethoscope$time, scoring_parameters()), {
        req(input_rv$ethoscope$data)
        output_rv$ethoscope <- score_monitor(input_rv$ethoscope, ethoscope_pb(), scoring_parameters(), FUN=FUNCTION_MAP$ethoscope)
      }, ignoreInit = TRUE)

      # Score DAM
      observeEvent(c(input_rv$dam$time, scoring_parameters()), {
        req(input_rv$dam$data)
        output_rv$dam <- score_monitor(input_rv$dan, dam_pb(), dam_scoring_parameters(), FUN=FUNCTION_MAP$dam)
      }, ignoreInit = TRUE)

      return(output_rv)
  })
}
