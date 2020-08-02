#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("mean", "median", "max", "min", "P_doze", "P_wake")

functions <- c(mean, median, max, min, fslsleepr::p_doze, fslsleepr::p_wake)
names(functions) <- FUN_choices

binDataUI <- function(id) {

  ns <- NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("summary_time_window"), label = "Summary time window",
                value = 30, min = 5, max = 120, step = 5),
    shiny::selectizeInput(ns("summary_FUN"), label = "Summary function", choices = FUN_choices),
    shiny::selectizeInput(inputId = ns("y"), label = "Variables", choices = "asleep",
                          multiple = TRUE, selected = "asleep")
  )
}

binDataServer <- function(id, grouped_data) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, name = NULL)

      # Update the rv object with the new binned time series
      observe({

        req(grouped_data$data)
        req(input$summary_time_window)
        req(input$summary_FUN)

        # if the passed variables to bin on are truthy
        # do this for all of them one by one:
        #
        # Bin the time series over time
        # with window length given by summary_time_window
        # and function given by summary_FUN
        if (shiny::isTruthy(input$y)) {
          data <- purrr::map(
            input$y,
            ~fslbehavr::bin_apply_all(
              grouped_data$data,
              .,
              x = "t",
              x_bin_length = fslbehavr::mins(input$summary_time_window),
              # TODO Support wrapping
              # wrap_x_by = time_wrap,
              FUN = functions[[input$summary_FUN]]
              )
          )

          # if more than one variable was passed,
          # merge the results
          # all together
          if (length(data) == 1) {
            rv$data <- data[[1]]
          } else {
            rv$data <- Reduce(x = data, f = fslbehavr::merge_behavr_all)
          }

        } else {
          rv$data <- grouped_data$data
        }
        rv$name <- grouped_data$name
      })


      # Update the list of binnable variables if the uploaded data changes
      var_choices <- reactive({

        if (! isTruthy(grouped_data$data)) {
          "asleep"
        } else {

          all_columns <- colnames(grouped_data$data)
          binnable_columns <- c("asleep", "moving", "interactions", "max_velocity", "is_interpolated", "beam_crosses", "x", "y")

          available_columns <- binnable_columns[
            purrr::map_lgl(
              binnable_columns,
              ~. %in% all_columns
            )
          ]
          available_columns
        }
      })

      observe({
        shiny::updateSelectizeInput(session, "y", choices = var_choices(), selected = input$y)
      })

      return(rv)
    }
  )
}
