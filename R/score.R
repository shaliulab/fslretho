FUNCTION_MAP <- list(
  "sleep_annotation" = fslsleepr::sleep_annotation
)

#' @importFrom shiny NS uiOutput
scoreDataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("scoringInput"))
}


#' Shiny module to automatically score a raw behavr table
#'
#' Provide a multi-animal reactive behavr and return the scored version
#'
#' @importFrom shiny moduleServer reactive observe eventReactive Progress
#' @importFrom fslbehavr bin_apply_all
#' @importFrom fslscopr annotate
#' @importFrom rlang fn_fmls
scoreDataServer <- function(id, dt_raw, dataset_name) {
  message("Executing scoreDataServer")

  moduleServer(

    id,
    function(input, output, session) {

      data <- reactiveValues(data = reactive(dt_raw()))

      message("Populating sidebar!")



      output$scoringInput <- renderUI({
        tagList(
          shiny::sliderInput(session$ns("velocity_correction_coef"), label = "", min = 0.001, max = 0.006, value = 0.004, step = 0.0001),
          shiny::sliderInput(session$ns("min_time_immobile"), label = "", min = 100, max = 600, value = 300, step = 10),
          shiny::sliderInput(session$ns("time_window_length"), label = "", min = 5, max = 60, value = 10, step = 5),
          shiny::selectizeInput(session$ns("FUN"), label = "", choices = c("sleep_annotation"))
        )
      })


      # the reason why we have a reactive list and not a list of reactives
      # is every expression that depends on of these parameters
      # also depends on the other two, i.e. they behave like a single unit
      user_input <- reactive({
        list(
          "velocity_correction_coef" = ifelse(is.null(input$velocity_correction_coef), 0.004, input$velocity_correction_coef),
          "min_time_immobile" = ifelse(is.null(input$min_time_immobile), 300, input$min_time_immobile),
          "time_window_length" = ifelse(is.null(input$time_window_length), 10, input$time_window_length)
          #, "motion_detector_FUN"
        )
      })

      user_functions <- reactive({
        ifelse(is.null(input$FUN), "sleep_annotation", input$FUN)
      })

      FUN <- reactive({

        passed_functions <- c()
        for (func in user_functions()) {
          FUN <- FUNCTION_MAP[[func]]
          FUN <- attr(FUN, "update")(user_input())
          passed_functions <- c(passed_functions, FUN)
        }
        passed_functions
      })

      # TODO Can this all be packaged into a function?
      dt <- reactive({

        dataset_name()
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "Scoring ", value = 0)
        # n <- nrow(dt_raw[, key(dt_raw), by = key(dt_raw)])
        # TODO make sure the below statement returns alwas the same
        n <- nrow(dt_raw()[, meta = T])

        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1 / n, detail = detail)
        }

        # TODO Find a way to either
        # * pass velocity_correction_coef only when FUN needs it or
        # * pass it always but have functions that don't complain about it being passed
        velocity_correction_coef <- user_input()$velocity_correction_coef
        dt <- fslscopr::annotate_all(data = dt_raw(), FUN = FUN(), updateProgress = updateProgress,
                                     velocity_correction_coef = velocity_correction_coef)
        dt
      })

      dt_validated <- reactive({
        validate(need(nrow(dt()) > 0, "Data cannot be annotated. This could be due to your dataset being sparse"))
        dt()
      })

      # # make it eager
      observe({
        data$data <<- reactive(dt_validated())
      })

      return(data)
    }
  )
}