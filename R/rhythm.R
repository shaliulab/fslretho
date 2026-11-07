#' Circadian rhythm analysis: periodograms and spectrograms (zeitgebr)

ALGORITHMS <- list(
  Periodogram = list(
    `Chi-square` = zeitgebr::chi_sq_periodogram,
    `Autocorrelation` = zeitgebr::ac_periodogram,
    `Lomb-Scargle` = zeitgebr::ls_periodogram
  ),
  Spectrogram = list(
    `Continuous wavelet transform` = zeitgebr::cwt_spectrogram
  )
)

#' Default plot of each analysis in esquisse
#' @noRd
RHYTHM_PLOTS <- list(
  Periodogram = list(
    hardcoded_dragula = list(mapping = list(xvar = "period", yvar = "signif_power"), geom = "pop_etho"),
    hardcoded_geom = "pop_etho",
    x_unit = c(period = "hours")
  ),
  Spectrogram = list(
    hardcoded_dragula = list(mapping = list(xvar = "t", yvar = "period"), geom = "tile_etho"),
    hardcoded_geom = "tile_etho",
    x_unit = c(t = "hours"),
    y_unit = c(period = "hours"),
    z_aes = "power"
  )
)

compute_rhythm <- function(type, data, var, algorithm, period_range, alpha = NULL) {
  FUN <- ALGORITHMS[[type]][[algorithm]]
  period_range <- behavr::hours(period_range)
  if (type == "Periodogram") {
    d <- zeitgebr::periodogram_standard(var = var, data = data, period_range = period_range, alpha = alpha, FUN = FUN)
    d[, signif_power := power - signif_threshold]
    zeitgebr::find_peaks(d)
  } else {
    zeitgebr::spectrogram_standard(var = var, data = data, period_range = period_range, FUN = FUN)
  }
}

rhythmAnalysisUI <- function(id, type) {
  ns <- NS(id)
  tagList(
    tags$h2(type),
    fluidRow(
      column(3, uiOutput(ns("y_ui"))),
      column(3, selectInput(ns("algorithm"), label = paste(type, "algorithm"), choices = names(ALGORITHMS[[type]]))),
      column(3, sliderInput(ns("period_range"), label = "Period range (hours)", min = 1, max = 48, value = c(16, 32), step = 1)),
      column(2, if (type == "Periodogram") sliderInput(ns("alpha"), label = "Significance level (alpha)", min = 0.001, max = 0.05, step = 0.001, value = 0.01)),
      column(1, actionButton(ns("button"), label = "Run", class = "btn-primary"))
    ),
    esquisseModuleUI(ns("esquisse"))
  )
}

#' @return dataset with the result, as filtered in the plot
#' @noRd
rhythmAnalysisServer <- function(id, input_rv, type) {
  moduleServer(
    id,
    function(input, output, session) {

      result <- dataset_rv()

      output$y_ui <- renderUI({
        data <- req(input_rv$data)
        numeric <- names(data)[sapply(data, is.numeric) & names(data) != "t"]
        selectInput(session$ns("y"), label = "Signal", choices = numeric, selected = if ("asleep" %in% numeric) "asleep")
      })

      observeEvent(input$button, {
        data <- req(input_rv$data)
        req(input$y)
        safely(context = paste("computing the", tolower(type)), {
          d <- compute_rhythm(type, data, input$y, input$algorithm, input$period_range, input$alpha)
          set_dataset(result, d, input_rv$name)
        })
      })

      do.call(esquisseModuleServer, c(list(id = "esquisse", input_rv = result), RHYTHM_PLOTS[[type]]))
    }
  )
}

periodAnalysisUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    rhythmAnalysisUI(ns("periodogram"), "Periodogram"),
    rhythmAnalysisUI(ns("spectrogram"), "Spectrogram")
  )
}

periodAnalysisServer <- function(id, input_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      list(
        periodogram = rhythmAnalysisServer("periodogram", input_rv, "Periodogram"),
        spectrogram = rhythmAnalysisServer("spectrogram", input_rv, "Spectrogram")
      )
    }
  )
}
