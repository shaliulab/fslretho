#' Inputs controlling how sleep is scored from the ethoscope tracking data
#'
#' The same three parameters exist twice: once for the whole experiment (default)
#' and once, optionally, for the sleep deprivation (SD) window
#' read from the ethoscope's SD schedule.
#' @param suffix "" for the default parameters, "_sd" for the SD ones
#' @noRd
scoringInputs <- function(ns, suffix = "") {
  id <- function(x) ns(paste0(x, suffix))
  tagList(
    sliderInput(
      id("velocity_correction_coef"), label = "Movement threshold (velocity correction coefficient)",
      min = 0.001, max = 0.006, value = 0.0048, step = 0.0001, width = "100%"
    ),
    sliderInput(
      id("min_time_immobile"), label = "Minimum time immobile to call sleep (s)",
      min = 60, max = 600, value = 300, step = 10, width = "100%"
    ),
    div(
      class = "fsl-inline-buttons",
      actionButton(id("time300"), label = "300 s (5 min rule)", class = "btn-xs"),
      actionButton(id("time60"), label = "60 s", class = "btn-xs")
    ),
    sliderInput(
      id("time_window_length"), label = "Window length (s)",
      min = 5, max = 60, value = 10, step = 5, width = "100%"
    ),
    numericInput(
      id("masking_duration"), label = "Ignore movement after an interaction (s)",
      value = DEFAULT_MASKING_DURATION, min = 0, max = 60, step = 1, width = "100%"
    ),
    helpText("The sleep depriver moves the tube, so the fly moves with it: that movement is ignored.",
             "Use 0 when the interaction is light (optogenetics), which does not move the tube.")
  )
}

#' Seconds of movement ignored after an interaction by sleepr's motion detector (masking_duration)
#' @noRd
DEFAULT_MASKING_DURATION <- 6

scoringParamsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "fsl-scoring",
    div(
      class = "fsl-scoring-col",
      tags$h4("Default parameters"),
      scoringInputs(ns),
      selectizeInput(
        ns("FUN"), label = "Annotations",
        choices = names(ANNOTATIONS),
        selected = c("sleep annotation", "SD in-progress annotation"),
        multiple = TRUE, width = "100%"
      ),
      checkboxInput(ns("curate"), label = "Discard windows with too little tracking data (curate)", value = TRUE),
      helpText("DAM data: only the minimum time immobile is used (a minute with any beam cross is a minute moving).")
    ),
    div(
      class = "fsl-scoring-col",
      tags$h4("During sleep deprivation (SD)"),
      checkboxInput(ns("differentiate_SD"), label = "Use different parameters during SD", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("differentiate_SD")),
        helpText(
          "These values replace the defaults while the SD is running,",
          "as set in the SD schedule (date range) of the ethoscope."
        ),
        scoringInputs(ns, suffix = "_sd")
      )
    )
  )
}

#' @return reactive list of arguments for scopr::load_ethoscope
#' @noRd
scoringParamsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      for (suffix in c("", "_sd")) local({
        # bind the values now: the observers run after the loop has finished
        slider <- paste0("min_time_immobile", suffix)
        button300 <- paste0("time300", suffix)
        button60 <- paste0("time60", suffix)
        observeEvent(input[[button300]], updateSliderInput(session, slider, value = 300))
        observeEvent(input[[button60]], updateSliderInput(session, slider, value = 60))
      })

      reactive({
        params <- list(
          velocity_correction_coef = req(input$velocity_correction_coef),
          min_time_immobile = req(input$min_time_immobile),
          time_window_length = req(input$time_window_length),
          curate = isTRUE(input$curate),
          FUN = unname(ANNOTATIONS[req(input$FUN)])
        )
        # only passed when changed, so the results cached with the default are reused
        masking <- function(value) if (is.null(value) || is.na(value)) DEFAULT_MASKING_DURATION else value
        if (masking(input$masking_duration) != DEFAULT_MASKING_DURATION) params$masking_duration <- masking(input$masking_duration)
        if (isTRUE(input$differentiate_SD)) {
          params$velocity_correction_coef_sd <- input$velocity_correction_coef_sd
          params$min_time_immobile_sd <- input$min_time_immobile_sd
          params$time_window_length_sd <- input$time_window_length_sd
          if (masking(input$masking_duration_sd) != masking(input$masking_duration)) params$masking_duration_sd <- masking(input$masking_duration_sd)
          # scopr::load_row reads the SD window from the ethoscope's SD schedule
          params$intervals <- list(interval_sd = "SD")
        }
        params
      })
    }
  )
}
