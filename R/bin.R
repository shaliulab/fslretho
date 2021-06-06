#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("sleep amount", "max", "min", "P_doze", "P_wake")

functions <- c(mean, median, max, min, fslsleepr::p_doze, fslsleepr::p_wake)
names(functions) <- FUN_choices

binDataUI <- function(id) {

  ns <- NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("summary_time_window"), label = "Summary time window",
                value = 30, min = 5, max = 120, step = 5),
    shiny::selectizeInput(ns("summary_FUN"), label = "Summary function", choices = FUN_choices),
    shiny::textInput(ns("y"), label = "Y axis", value="asleep")
  )
}


#' @import fslbehavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, grouped_data, preproc_FUN=NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      x_bin_length <- reactive({
        ifelse(is.null(input$summary_time_window), behavr::mins(30), input$summary_time_window)
      })

      FUN <- reactive({
        functions[ifelse(is.null(input$summary_FUN), "sleep_amount", input$summary_FUN)]
      })

      data <- reactive({

        if (is.null(preproc_FUN)) {
          grouped_data$data
        } else {
          preproc_FUN(grouped_data$data, ...)
        }
      })

      binned_data <- reactive({
        behavr::bin_apply_all(
          data(),
          x = "t",
          y = input$y,
          x_bin_length = x_bin_length(),
          FUN = FUN()
        )
      })

      return(binned_data)
    }
  )}


bout_analysis <- function(data, ...) {
  sleepr::bout_analysis(data = data, ...)
}