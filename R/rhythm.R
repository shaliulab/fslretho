#' Cyrcadian rhythm functionality in FSLRetho
#'

PERIODOGRAM_ALGORITHMS <- list(
  Chisq = zeitgebr::chi_sq_periodogram,
  autocorrelation = zeitgebr::ac_periodogram,
  `Lomb-Scargle` = zeitgebr::ls_periodogram
)

SPECTROGRAM_ALGORITHMS <- list(
  CWT = zeitgebr::cwt_spectrogram
)

ALGORITHMS <- list(
  Periodogram = PERIODOGRAM_ALGORITHMS,
  Spectrogram = SPECTROGRAM_ALGORITHMS
)


periodAnalysisUI <- function(id) {

  ns <- NS(id)

  wellPanel(
    analysisUI(ns("periodogram"), module = "Periodogram"),
    analysisUI(ns("spectrogram"), module = "Spectrogram")
  )
}


analysisUI <- function(id, module) {

  ns <- NS(id)

  if (module == "Periodogram") {
    alpha <- sliderInput(ns("alpha"), label = "Alpha (significance)", min = 0.001, max = 0.01, step = 0.001, value = 0.01)
  } else {
    alpha <- NULL
  }

  tagList(
    tags$h2(paste0(module, " Inputs")),
    uiOutput(ns("y_ui")),
    selectInput(ns("algorithm"), label = "Periodogram algorithm",
                choices = names(ALGORITHMS[[module]])
    ),
    tags$h3("Advanced settings"),
    alpha,
    # sliderInput(ns("resample_period"), label = "Resample period (mins)", min = 1, max = 30, step = 1, value = 15),
    sliderInput(ns("period_range"), label = "Period range (hours)", min = 1, max = 48, value = c(16, 32), step = 1),
    esquisseModuleUI(ns("esquisse"))
  )
}


#' @import zeitgebr
periodogramAnalysisServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      esquisse_rv <- reactiveValues(data=NULL, name=NULL, time=NULL)

      numeric_columns <- reactive({
        columns <- colnames(req(input_rv$data))
        columns <- columns[columns != "t"]
        columns <- sapply(columns, function(x) {class(input_rv$data[[x]]) %in% c("integer", "numeric")})
        names(columns)[columns]
      })

      y_ui <- reactive({
        selectInput(session$ns("y"), label = "Signal", choices = numeric_columns(), selected = "")
      })
      # make y_ui eager
      # TODO needed?
      observe({y_ui()})
      output$y_ui <- renderUI({y_ui()})


      dt <- reactive({

        req(input$y)
        req(input_rv$data)
        req(input$period_range)
        req(input$algorithm)
        req(input$alpha)
        input_rv$time

        d <- zeitgebr::periodogram_standard(
          var=input$y,
          data=input_rv$data,
          period_range = behavr::hours(input$period_range),
          # resample_rate = 1 / behavr::mins(input$resample_period),
          alpha = input$alpha,
          FUN=PERIODOGRAM_ALGORITHMS[[input$algorithm]]
        )

        d <- zeitgebr::find_peaks(d)
        d
      })

      observe({
        req(dt())
        esquisse_rv$data <- dt()
        esquisse_rv$name <- input_rv$name
        esquisse_rv$time <- Sys.time()
        message("Outputing periodogram data")
      })

      output_rv <- esquisseModuleServer("esquisse", input_rv = esquisse_rv,
                                        hardcoded_dragula = list(
                                            mapping = list(xvar = "period", yvar = "power"),
                                            geom = "pop_etho"
                                        ),
                                        hardcoded_geom = "pop_etho",
                                        x_unit = c(period = "hours"))

      return(output_rv)
    })
}



#' @import zeitgebr
spectrogramAnalysisServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      esquisse_rv <- reactiveValues(data=NULL, name=NULL, time=NULL)

      numeric_columns <- reactive({
        columns <- colnames(req(input_rv$data))
        columns <- columns[columns != "t"]
        columns <- sapply(columns, function(x) {class(input_rv$data[[x]]) %in% c("integer", "numeric")})
        names(columns)[columns]
      })

      y_ui <- reactive({
        selectInput(session$ns("y"), label = "Signal", choices = numeric_columns(), selected = "")
      })
      # make y_ui eager
      # TODO needed?
      observe({y_ui()})
      output$y_ui <- renderUI({y_ui()})


      dt <- reactive({

        req(input$y)
        req(input_rv$data)
        req(input$period_range)
        req(input$algorithm)

        zeitgebr::spectrogram_standard(
          var=input$y,
          data=input_rv$data,
          period_range = behavr::hours(input$period_range),
          # resample_rate = 1 / behavr::mins(input$resample_period),
          FUN=SPECTROGRAM_ALGORITHMS[[input$algorithm]]
        )
      })

      observeEvent(input_rv$time, {
        esquisse_rv$data <- dt()
        esquisse_rv$name <- input_rv$name
        esquisse_rv$time <- input_rv$time

      }, ignoreInit = TRUE)


      observe({
        req(dt())
        output_rv$data <- dt()
        output_rv$name <- input_rv$name
        output_rv$time <- Sys.time()
        message("Outputing periodogram data")
      })

      output_rv <- esquisseModuleServer("esquisse", input_rv = output_rv,
                                        hardcoded_dragula = list(
                                          mapping = list(xvar = "t", yvar = "period"),
                                          geom = "tile_etho"
                                        ),
                                        hardcoded_geom = "tile_etho",
                                        x_unit = c(t = "hours"),
                                        y_unit = c(period = "hours"),
                                        z_aes = "power")

      return(output_rv)
    })
}

periodAnalysisServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        periodogram = reactiveValues(data = NULL, name = NULL, time = NULL),
        spectrogram = reactiveValues(data = NULL, name = NULL, time = NULL)
      )

      periodogram_rv <- periodogramAnalysisServer("periodogram", input_rv)
      spectrogram_rv <- spectrogramAnalysisServer("spectrogram", input_rv)

      observeEvent(periodogram_rv$time, {
        output_rv$periodogram$data <- periodogram_rv$data
        output_rv$periodogram$name <- periodogram_rv$name
        output_rv$periodogram$time <- periodogram_rv$time
      }, ignoreInit = TRUE)


      observeEvent(spectrogram_rv$time, {
        output_rv$spectrogram$data <- spectrogram_rv$data
        output_rv$spectrogram$name <- spectrogram_rv$name
        output_rv$spectrogram$time <- spectrogram_rv$time
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}

