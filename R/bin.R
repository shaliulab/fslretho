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

      rv <- reactiveValues(date = NULL, name = NULL)

      observe({

        req(grouped_data$data)
        req(input$summary_time_window)
        req(input$summary_FUN)

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

      var_choices <- reactive({

        if (! isTruthy(grouped_data$data)) {
          "asleep"
        } else {

          all_columns <- colnames(grouped_data$data)
          binnable_columns <- c("asleep", "moving", "interactions", "max_velocity", "is_interpolated", "beam_crosses", "x", "y")

          res <- binnable_columns[
            purrr::map_lgl(
              binnable_columns,
              ~. %in% all_columns
            )
          ]

          res

        }
      })

      observe({
        shiny::updateSelectizeInput(session, "y", choices = var_choices(), selected = input$y)
      })


      return(rv)
    }
  )
}
