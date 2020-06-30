#' Aggregate a categorical data over a fixed period of time
#'
#' Given a statistic function and a fixed window length,
#' `summariseBehavr` generates a new behavr table where the original
#' time series is aggregated with that window length and a new column
#' with the result of applying the statistic function to a categorical variable
#' This behavr table and other parameters useful for plotting the time series
#' are returned in a list
#'
#' @param input A shiny input object
#' @param output A shiny output object
#' @param session A shiny session object
#' @param dt A behavr table with a categorical column to be summarised
#' @param feature Name of the categorical column
#' @return A list with elements
#' \itemize{
#'  \item data The summarised behavr table
#'  \item mapping
#' }
#' @importFrom ggplot2 aes
#' @importFrom fslbehavr mins
summariseBehavrServer <- function(id, dt, feature = "asleep") {
  moduleServer(
    id,
    function(input, output, session) {

      asleep <- NULL

      # TODO Provide a way to change the summary_FUN and the summary_time_window
      # TODO Find a way to change the column mapped to y without changing its name to something static
      ggetho_input <- reactive({
        ggetho_input <- fslggetho::ggetho_preprocess(
          data = dt(),
          mapping = aes(x = t, y = asleep),
          summary_FUN = mean,
          summary_time_window = fslbehavr::mins(30)
        )
        ggetho_input
      })
      return(ggetho_input)
    }
  )
}

plotBehavrServer <- function(id, ggetho_input) {
  moduleServer(
    id,

    function(input, output, session) {

      gg <- reactive({

        gg <- fslggetho::ggetho_plot(
        # gg <- ggetho_plot(
          data = ggetho_input()$dt,
          mapping = ggetho_input()$mapping,
          scale_x_FUN = ggetho_input()$scale_x_FUN,
          discrete_y = ggetho_input()$discrete_y,
          time_offset = ggetho_input()$time_offset
        )
        gg <- gg + fslggetho::stat_pop_etho()
        browser()
        gg
      })
      return(gg)
    }
  )
}




analyseSleepServer <- function(id, scored_data, dataset_name) {
  moduleServer(
    id,
    function(input, output, session) {

      # browser()

      ggetho_input <- summariseBehavrServer("summariseBehavr", scored_data, feature = "asleep")

      output$summarised_data <- downloadHandler(
        filename = function() {
          paste0(dataset_name(), ".csv")
        }, content = function(file) {
          data <- ggetho_input()$dt
          data[, file_info := purrr::map(file_info, ~.[["path"]])]
          data.table::fwrite(x = data, file, row.names=FALSE)
        }
      )

      output$plot1 <- renderPlot({
        plotBehavrServer("plotBehavr", ggetho_input)()
      })
    }
  )
}

analyseSleepUI <- function(id) {

  ns <- NS(id)

  shiny::fluidRow(
    shinydashboard::box(title = 'Sleep trace', status = 'primary',
      shiny::plotOutput(ns("plot1"), height = 250)
    ),
      shiny::downloadButton(ns("summarised_data"))
  )
}