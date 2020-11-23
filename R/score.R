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


score_monitor <- function(raw_data, input, monitor) {

  # reactive({
  # TODO Can this be a reactiveValues?
    # user_input <- reactive({list(
    # browser()

  rv <- reactiveValues(
    data = NULL,
    name = NULL,
    time = NULL
  )


  if (is.null(raw_data[[monitor]]$data)) {
    return(rv)
  }

  user_input <- list(
        "velocity_correction_coef" = ifelse(is.null(input$velocity_correction_coef), 0.004, input$velocity_correction_coef),
        "min_time_immobile" = ifelse(is.null(input$min_time_immobile), 300, input$min_time_immobile),
        "time_window_length" = ifelse(is.null(input$time_window_length), 10, input$time_window_length)
      )
    # })

    req(input$velocity_correction_coef)
    req(input$min_time_immobile)
    req(input$time_window_length)
    # req(raw_data[[monitor]]$time)
    print(raw_data[[monitor]]$time)
    req(raw_data[[monitor]]$time)

    if (!isTruthy(raw_data[[monitor]]$data)) return(NULL)
    FUNCTION_MAP <- list(
      "sleep_annotation" = list(
        "ethoscope" = fslsleepr::sleep_annotation,
        "dam" = fslsleepr::sleep_dam_annotation
      )
    )

    passed_function <- FUNCTION_MAP$sleep_annotation[[monitor]]
    req(passed_function)
    scoring_function <- attr(passed_function, "updater")(user_input)

    progress <- shiny::Progress$new()
    on.exit(progress$close())

    progress$set(message = "Scoring ", value = 0)
    # TODO make sure the below statement returns alwas the same
    n <- nrow(raw_data[[monitor]]$data[, meta = T])

    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1 / n, detail = detail)
    }

    if (isTruthy(raw_data[[monitor]]$data)) {
      data_annotated <- fslscopr::annotate_all(data = raw_data[[monitor]]$data, FUN = scoring_function, updateProgress = updateProgress)
      validate(need(nrow(data_annotated) > 0, "Data cannot be annotated. This could be due to your dataset being sparse"))
      rv$data <- data_annotated
      rv$name <- raw_data[[monitor]]$name
      rv$time <- raw_data[[monitor]]$time
    } else {
      rv$data <- NULL
      rv$name <- NULL
      rv$time <- NULL
    }

    rv
}


#' Shiny module to automatically score a raw behavr table
#'
#' Provide a multi-animal reactive behavr and return the scored version
#'
#' @param id Module id - character
#' @param raw_data A shiny reactiveValues with slots data and name
#' @importFrom shiny moduleServer reactive observe eventReactive Progress
#' @importFrom fslbehavr bin_apply_all
#' @importFrom fslscopr annotate
#' @importFrom rlang fn_fmls
scoreDataServer <- function(id, raw_data) {

  moduleServer(

    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

      # last_monitor <- reactive({
      #   req(raw_data$data)
      #   attr(raw_data$data, "monitor")
      # })

      # Convert the user passed character strings
      # into the actual functions
      # scoring_function <- reactive({
      #
      #   req(input$FUN)
      #
      #   passed_functions <- c()
      #   for (func in input$FUN) {
      #   passed_function <- FUNCTION_MAP[[func]]
      #     if (func %in% monitor_sensitive) passed_function <- passed_function[[last_monitor()]]
      #     passed_function <- attr(passed_function, "updater")(user_input())
      #     passed_functions <- c(passed_functions, passed_function)
      #   }
      #   passed_functions
      # })

      # at least data from one monitor must be available
      observe({
        # browser()
        req(c(raw_data$ethoscope, raw_data$dam))
      })

      # TODO If this is done with BiocParallel, these would be loaded in parallel
      # On the other hand, loading DAM is very fast, so it's not really needed

      monitors_dt <- reactiveValues(ethoscope = NULL, dam = NULL)

      observe({
        monitors_dt$ethoscope <- score_monitor(raw_data, input, "ethoscope")
        monitors_dt$dam <- score_monitor(raw_data, input, "dam")
      })
      return(monitors_dt)
    }
  )
}