FUNCTION_MAP <- list(
  "sleep_annotation" = list(
    "ethoscope" = fslsleepr::sleep_annotation,
    "dam" = fslsleepr::sleep_dam_annotation
    )
)

monitor_sensitive <- names(which(unlist(lapply(FUNCTION_MAP, length)) == 2))


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

      rv <- reactiveValues(data = NULL, name = NULL)

      # output$scoringInput <- renderUI({
      #   tagList(
      #     shiny::sliderInput(session$ns("velocity_correction_coef"), label = "Threshold (velocity correction coef)", min = 0.001, max = 0.006, value = 0.004, step = 0.0001),
      #     shiny::sliderInput(session$ns("min_time_immobile"), label = "Mimimum time immobile", min = 100, max = 600, value = 300, step = 10),
      #     shiny::sliderInput(session$ns("time_window_length"), label = "Window duration", min = 5, max = 60, value = 10, step = 5),
      #     shiny::selectizeInput(session$ns("FUN"), label = "", choices = c("sleep_annotation"))
      #   )
      # })

      # TODO Can this be a reactiveValues?
      user_input <- reactive({
        list(
          "velocity_correction_coef" = ifelse(is.null(input$velocity_correction_coef), 0.004, input$velocity_correction_coef),
          "min_time_immobile" = ifelse(is.null(input$min_time_immobile), 300, input$min_time_immobile),
          "time_window_length" = ifelse(is.null(input$time_window_length), 10, input$time_window_length)
        )
      })

      last_monitor <- reactive({
        req(raw_data$data)
        attr(raw_data$data, "monitor")
      })

      # Convert the user passed character strings
      # into the actual functions
      scoring_function <- reactive({

        req(input$FUN)

        passed_functions <- c()
        for (func in input$FUN) {
        passed_function <- FUNCTION_MAP[[func]]
          if (func %in% monitor_sensitive) passed_function <- passed_function[[last_monitor()]]
          passed_function <- attr(passed_function, "updater")(user_input())
          passed_functions <- c(passed_functions, passed_function)
        }
        passed_functions
      })

      dt <- reactive({

        req(raw_data$data)
        req(input$velocity_correction_coef)
        req(input$min_time_immobile)
        req(input$time_window_length)
        req(scoring_function())

        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "Scoring ", value = 0)
        # TODO make sure the below statement returns alwas the same
        n <- nrow(raw_data$data[, meta = T])

        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1 / n, detail = detail)
        }

        fslscopr::annotate_all(data = raw_data$data, FUN = scoring_function(), updateProgress = updateProgress)
      })

      dt_validated <- reactive({
        validate(need(nrow(dt()) > 0, "Data cannot be annotated. This could be due to your dataset being sparse"))
        dt()
      })

      observe({
        rv$data <- dt_validated()
        rv$name <- raw_data$name
      })

      return(rv)
    }
  )
}