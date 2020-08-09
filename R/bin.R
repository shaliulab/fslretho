#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("mean", "max", "min", "P_doze", "P_wake")

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


#' @import fslbehavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, grouped_data, summary_time_window = NULL, main = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, name = NULL, summary_FUN = NULL, y = NULL)

      # Update the rv object with the new binned time series
      observe({

        req(grouped_data$data)
        req(input$summary_time_window)
        req(input$summary_FUN)
        summary_FUN <- functions[[input$summary_FUN]]

        x_bin_length <- ifelse(
          is.null(summary_time_window),
          fslbehavr::mins(input$summary_time_window),
          fslbehavr::mins(summary_time_window)
        )

        available_columns <- input$y %in% colnames(grouped_data$data)
        if (!all(available_columns)) {
          warning(sprintf("The following columns to be binned are NOT available %s",
                          paste0(input$y[!available_columns], sep = ", ")
                          )
          )
        }

        y <- input$y[available_columns]

        # if the passed variables to bin on are truthy
        # do this for all of them one by one:
        #
        # Bin the time series over time
        # with window length given by summary_time_window
        # and function given by summary_FUN


        # copy to avoid the reactivevalue to be processed several times
        data <- data.table::copy(grouped_data$data)

        if (shiny::isTruthy(input$y)) {

          print(key(grouped_data$data))
          rv$data <- fslbehavr::bin_all(
            data = grouped_data$data,
            y = y,
            x_bin_length = x_bin_length,
            FUN = summary_FUN
          )

        } else {
          rv$data <- grouped_data$data
        }
        rv$name <- grouped_data$name
        rv$summary_FUN <- summary_FUN
        rv$y <- y
      })


      # update the UI only if main is TRUE
      # this is to avoid the different instances of the module
      # which share the UI from stepping on each other
      # only the main instance writes to the UI
      # (even if all the instances read from it)
      if (isTRUE(main)) {
        # Update the list of binnable variables if the uploaded data changes
        var_choices <- reactive({

          req(grouped_data$data)

          all_columns <- colnames(grouped_data$data)
          binnable_columns <- c("asleep", "moving", "interactions", "max_velocity", "is_interpolated", "beam_crosses", "x", "y")

          available_columns <- binnable_columns[
            purrr::map_lgl(
              binnable_columns,
              ~. %in% all_columns
            )
          ]
          available_columns

        })

        selected <- reactive({
          req(grouped_data$data)
          output <- c(input$y)

          if ("interactions" %in% colnames(grouped_data$data)) {
            output <- c(output, "interactions")
          }

          output
        })

        observe({
          shiny::updateSelectizeInput(session, "y", choices = var_choices(), selected = isolate(selected()))
        })
      }
      return(rv)
    }
  )
}
