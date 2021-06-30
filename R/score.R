#' Annotate a behavr with a SD treatment progress
#' A new `sd_on` column stating the SD treatment status is added to the passed behavr
#' @param data behavr table
#' @return the same behavr table with a new column `sd_on`.
#' @export
sd_inprogress_annotation <- function(data, time_window_length=10,...) {

  d <- sleepr::prepare_data_for_motion_detector(data, c("id", "t"), time_window_length=time_window_length)
  d_small <- d[!duplicated(t_round), ]
  d_small$t <- NULL
  data.table::setnames(d_small, "t_round", "t")
  # d_small must have keys like the data in a behavr
  data.table::setkey(d_small, id)
  d_small <- parse_sd_daterange(d_small)
  d_small[, sd_on := ((t > xmv(start_sd)) & (t < xmv(end_sd)))]
  return(d_small)
}
attr(sd_inprogress_annotation, "needed_columns") <- function() {
  c("t")
}
attr(sd_inprogress_annotation, "variables") <- function() {
  c("sd_on")
}
attr(sd_inprogress_annotation, "parameters") <- function() {
  c()
}


FUNCTION_MAP <- list(
  `sleep annotation` = list(ethoscope = sleepr::sleep_annotation, dam = sleepr::sleep_dam_annotation),
  `distance annotation` = list(ethoscope = sleepr::sum_movement_detector, dam = NULL),
  `SD in-progress annotation` = list(ethoscope = sd_inprogress_annotation, dam = NULL)
)

conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug


#' @importFrom shiny NS uiOutput
scoreDataUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::sliderInput(ns("velocity_correction_coef"), label = "Threshold (velocity correction coef)", min = 0.001, max = 0.006, value = 0.0048, step = 0.0001),
    shiny::sliderInput(ns("min_time_immobile"), label = "Mimimum time immobile", min = 100, max = 600, value = 300, step = 10),
    shiny::sliderInput(ns("time_window_length"), label = "Window duration", min = 5, max = 60, value = 10, step = 5),
    shiny::selectizeInput(
      ns("FUN"), label = "",
      choices = c("sleep annotation", "distance annotation", "SD in-progress annotation"),
      selected = c("sleep annotation", "SD in-progress annotation"),
      multiple=TRUE
    )
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

  scoring_parameters <- list(...)

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

      progress_bar <- reactiveValues(progress = NULL, update = NULL)

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


      observeEvent(input_rv$dam$data, {
        req(input_rv$dam$data)
        n_individuals <- nrow(input_rv$dam$data[, meta = T])
        pb <- get_progress_bar(n_individuals, "Scoring")
        progress_bar$progress <- pb$progress
        progress_bar$update <- pb$update
      })


      FUN <- reactive({
        lapply(input$FUN, function(f) FUNCTION_MAP[[f]]$ethoscope)
      })

      # Score ethoscope
      observeEvent(c(input_rv$ethoscope$time, scoring_parameters(), FUN()), {

        req(input_rv$ethoscope$data)
        req(FUN())
        req(scoring_parameters())

        n_individuals <- nrow(input_rv$ethoscope$data[, meta = T])
        pb <- get_progress_bar(n_individuals, "Scoring")
        progress_bar$progress <- pb$progress
        progress_bar$update <- pb$update

        args <- append(list(input_rv=input_rv$ethoscope, updateProgress = progress_bar$update, FUN = FUN()), scoring_parameters())
        output_rv$ethoscope <- do.call(score_monitor, args)
        output_rv$ethoscope$time <- Sys.time()
        # just a sapply returns a matrix, so I need lapply and unlist
        output_rv$ethoscope$variables <- tryCatch(unlist(lapply(FUN(), function(f) {attr(f, "variables")()})), error = function(e) {"asleep"})
        on.exit(progress_bar$progress$close())
      }, ignoreInit = TRUE)

      # Score DAM
      observeEvent(c(input_rv$dam$time, scoring_parameters()), {
        req(input_rv$dam$data)
        output_rv$dam <- score_monitor(input_rv$dan, progress_bar$update, dam_scoring_parameters(), FUN=FUNCTION_MAP$dam)
        on.exit(progress_bar$progress$close())
      }, ignoreInit = TRUE)

      return(output_rv)
  })
}
