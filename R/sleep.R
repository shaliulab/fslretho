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
summariseBehavrServer <- function(id, scored_data, feature = "asleep") {
  moduleServer(
    id,
    function(input, output, session) {

      stopifnot(isolate(is.reactive(scored_data$data)))
      stopifnot(isolate(!is.reactive(feature)))

      asleep <- NULL
      # TODO Provide a way to change the summary_FUN and the summary_time_window
      # TODO Find a way to change the column mapped to y
      # without changing its name to something static
      ggetho_input <- reactive({
          fslggetho::ggetho_preprocess(
            data = scored_data$data(),
            mapping = ggplot2::aes(x = t, y = asleep),
            summary_FUN = mean,
            summary_time_window = fslbehavr::mins(30)
          )
      })

      output <- list(
        data = reactive(ggetho_input()$dt),
        mapping = reactive(ggetho_input()$mapping),
        scale_x_FUN = reactive(ggetho_input()$scale_x_FUN),
        discrete_y = reactive(ggetho_input()$discrete_y),
        time_offset = reactive(ggetho_input()$time_offset)
      )
      return(output)
    }
  )
}


plotBehavrUI <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    shinydashboard::box(
      title = "Sleep trace", status = "primary",
      shiny::plotOutput(ns("plot"))
    ),
    shiny::downloadButton(ns("download"))
  )
}

plotBehavrServer <- function(id, ggetho_input, dataset_name) {
  moduleServer(
    id,

    function(input, output, session) {

      stopifnot(!is.reactive(ggetho_input))
      stopifnot(is.reactive(ggetho_input$data))

      plot <- reactiveValues(plot = reactive(ggplot2::ggplot()))

      gg <- reactive({

        gg <- fslggetho::ggetho_plot(
          data = ggetho_input$data(),
          mapping = ggetho_input$mapping(),
          scale_x_FUN = ggetho_input$scale_x_FUN(),
          discrete_y = ggetho_input$discrete_y(),
          time_offset = ggetho_input$time_offset()
        )
        gg <- gg + fslggetho::stat_pop_etho(aes(fill = group, color = group))
        gg
      })

      observe({
        plot$plot <<- reactive(gg())
      })

      output$plot <- renderPlot({
        plot$plot()
      })

      # outputOptions(output, "plot", suspendWhenHidden = FALSE)

      output$download <- downloadHandler(
        filename = function() {
          datetime_filename(paste0(dataset_name(), ".png"))
        }, content = function(file) {

          ggplot2::ggsave(plot = plot$plot(), filename = file)
        }
      )
    }
  )
}



analyseSleepUI <- function(id) {

  ns <- NS(id)

  shiny::fluidRow(
    plotBehavrUI(ns("plotBehavr")),
    shiny::downloadButton(ns("summarised_data"))
  )
}


analyseSleepServer <- function(id, scored_data, dataset_name) {
  moduleServer(
    id,
    function(input, output, session) {

      stopifnot(is.reactive(isolate(scored_data$data)))
      stopifnot(is.reactive(isolate(dataset_name)))

      ggetho_input <- summariseBehavrServer(
        "summariseBehavr",
        scored_data,
        feature = "asleep"
      )

      plotBehavrServer("plotBehavr", ggetho_input, dataset_name)

      output$summarised_data <- downloadHandler(
        filename = function() {
          datetime_filename(paste0(dataset_name(), ".csv"))
        }, content = function(file) {

          data <- fortify(ggetho_input$data())
          data.table::fwrite(x = data, file, row.names = FALSE)
        }
      )
    }
  )
}