#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("mean", "median", "max", "min", "P_doze", "P_wake")
PARETO <- TRUE
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
                   ),
    tags$h2("Quality refinement"),
    checkboxInput(ns("pareto"), label = "Apply pareto principle", value = FALSE),
    checkboxInput(ns("pareto_sd"), label = "If pareto is applied, should it be during SD only?", value = TRUE)

    # uiOutput(ns("y_ui"))
  )
}


#' @param y column to bin over, overrides whatever the user may pass in the input
#' @param summary_time_window width of the bins, in minutes, overrides whatever the user may pass in the input
#' @import behavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, input_rv, y = NULL, summary_time_window = NULL, summary_FUN = NULL, preproc_FUN=NULL, allow_pareto=FALSE, ...) {

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

      variables <- reactive({

        req(preproc_data())

        x <- input_rv$variables
        if (!is.null(preproc_FUN)) {
          x <- c(attr(preproc_FUN, "variables")(), x)
        }
        x <- x[x %in% colnames(preproc_data())]
        x

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
        message("Updataing bin-y")
        updateSelectizeInput(inputId = "y", choices = variables(), selected = variables()[1])
      }, ignoreInit = TRUE)

      observeEvent(c(input_rv$time, input$summary_FUN, input$summary_time_window, input$pareto, input$y), {

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

        FUN <- functions[[ifelse(is.null(summary_FUN), input$summary_FUN, summary_FUN)]]

        binned_dataset <- behavr::bin_all(
          data = preproc_data(),
          y = y_passed,
          x = "t",
          x_bin_length = behavr::mins(ifelse(is.null(summary_time_window), input$summary_time_window, summary_time_window)),
          FUN = FUN
        )


        if (allow_pareto & input$pareto) {

          binned_dataset <- apply_pareto_rule(
            preproc_data(), binned_dataset,
            x_bin_length = behavr::mins(ifelse(is.null(summary_time_window), input$summary_time_window, summary_time_window)),
            sd_only=input$pareto_sd
          )

          # pareto_dataset <- behavr::bin_all(
          #   data = preproc_data(),
          #   y = "x",
          #   x = "t",
          #   x_bin_length = behavr::mins(ifelse(is.null(summary_time_window), input$summary_time_window, summary_time_window)),
          #   FUN = pareto_sd
          # )
          #
          # setkey(binned_dataset, id, t)
          # setkey(pareto_dataset, id, t)
          # merged_dataset <- merge_behavr_all(binned_dataset, pareto_dataset)
          # merged_dataset[, asleep := sapply(as.numeric(pareto * 1) + asleep, function(a) min(1, a))]
          # setkey(merged_dataset, id)
          # binned_dataset <- merged_dataset
        }

        output_rv$data <- binned_dataset
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
attr(bout_analysis, "variables") <- function() {"duration"}