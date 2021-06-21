#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("mean", "median", "max", "min", "P_doze", "P_wake")

functions <- list(mean, median, max, min, sleepr::p_doze, sleepr::p_wake)
names(functions) <- FUN_choices

conf <- FSLRethoConfiguration$new()
DEBUG <- TRUE

binDataUI <- function(id, binning_variable="asleep") {

  ns <- NS(id)
  tagList(
    sliderInput(ns("summary_time_window"), label = "Summary time window",
                value = 30, min = 5, max = 120, step = 5),
    selectizeInput(ns("summary_FUN"), label = "Summary function", choices = FUN_choices, selected = "mean"),
    # textInput(ns("y"), label = "Y axis", value=binning_variable),
    selectizeInput(inputId = ns("y"), label = "Y axis", choices = binning_variable,
                   multiple=TRUE,
                   selected = binning_variable
                   )
    # uiOutput(ns("y_ui"))
  )
}


#' @param y column to bin over, overrides whatever the user may pass in the input
#' @param summary_time_window width of the bins, in minutes, overrides whatever the user may pass in the input
#' @import behavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, input_rv, y = NULL, summary_time_window = NULL, summary_FUN = NULL, preproc_FUN=NULL, ...) {

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

      # output$y_ui <- renderUI({
      #   message("Rendeing UI")
      #   input_rv$time
      # })

      input_y <- reactive(input$y)
      observe({
        input_y()
      })

      observeEvent(input_rv$time, {
        updateSelectizeInput(inputId = "y", choices = input_rv$variables, selected = input_rv$variables[1])
      }, ignoreInit = TRUE)

      observeEvent(c(input_rv$time, input$summary_FUN, input$summary_time_window, input$y), {

        req(input_rv$data)
        req(input$y)
        # if (length(input$y) > 1) browser()
        if (DEBUG) message(paste0("Binning data using ", input$summary_FUN))

        if (is.null(y))
          y_passed <- input_y()
        else
          y_passed <- y

        kept_y <- y_passed %in% colnames(preproc_data())
        y_passed <- y_passed[kept_y]
        if (!all(kept_y) & sum(kept_y) > 0)
          warning("Some variables are not in the data")

        req(any(kept_y))

        binned_dataset <- behavr::bin_all(
          data = preproc_data(),
          y = y_passed,
          x = "t",
          x_bin_length = behavr::mins(ifelse(is.null(summary_time_window), input$summary_time_window, summary_time_window)),
          FUN = functions[[ifelse(is.null(summary_FUN), input$summary_FUN, summary_FUN)]]
        )


        rejoined_dataset <- behavr::rejoin(binned_dataset)
        output_rv$data <- rejoined_dataset
        output_rv$name <- input_rv$name
        output_rv$time <- Sys.time()
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )}

#' A version of sleepr::bout_analysis that allows programmatic input
#' @seealso sleepr::bout_analysis
#' @inheritParams sleepr::bout_analysis
#' @export
bout_analysis <- function(data, ...) {
  sleepr::bout_analysis_standard(data = data, ...)
}