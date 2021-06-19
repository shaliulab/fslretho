#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("sleep amount", "max", "min", "P_doze", "P_wake")

functions <- list(mean, median, max, min, sleepr::p_doze, sleepr::p_wake)
names(functions) <- FUN_choices

conf <- FSLRethoConfiguration$new()
DEBUG <- TRUE

binDataUI <- function(id) {

  ns <- NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("summary_time_window"), label = "Summary time window",
                value = 30, min = 5, max = 120, step = 5),
    shiny::selectizeInput(ns("summary_FUN"), label = "Summary function", choices = FUN_choices, selected = "sleep amount"),
    shiny::textInput(ns("y"), label = "Y axis", value="asleep")
  )
}


#' @import behavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, input_rv, preproc_FUN=NULL, ...) {

  output_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

  moduleServer(
    id,
    function(input, output, session) {

      preproc_data <- reactive({

        if (is.null(preproc_FUN)) {
          # just use the data as is
          input_rv$data
        } else {
          # preprocess it
          preproc_FUN(data=input_rv$data, ...)
        }
      })

      observeEvent(c(input_rv$time, input$summary_FUN, input$summary_time_window), {

        req(input_rv$data)
        if (DEBUG) message(paste0("Binning data using ", input$summary_FUN))
        binned_dataset <- behavr::bin_apply_all(
          preproc_data(),
          x = "t",
          y = input$y,
          x_bin_length = behavr::mins(input$summary_time_window),
          FUN = functions[[input$summary_FUN]]
        )

        rejoined_dataset <- behavr::rejoin(binned_dataset)
        rejoined_dataset$target_ <- rejoined_dataset[[input$y]]
        output_rv$data <- rejoined_dataset
        output_rv$name <- input_rv$name
        output_rv$time <- input_rv$time
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )}


bout_analysis <- function(data, ...) {
  sleepr::bout_analysis_standard(data = data, ...)
}