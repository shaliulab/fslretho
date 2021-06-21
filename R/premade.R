premadePlotsUI <- function(id) {

  ns <- NS(id)

  tagList(
    plotOutput(ns("plot_sleep")),
    plotOutput(ns("plot_interactions"))
  )
}

#' Premade plots without any user tweaking
#'
#' @param input_rv reactiveValues with a data slot containing an ethoscope behavr table
#' @import ggetho
premadePlotsServer <- function(id, sleep_rv, interactions_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      sleep_data <- eventReactive(sleep_rv$time, {
        req(sleep_rv$data)
        sleep_rv$data
      })

      interactions_data <- eventReactive(interactions_rv$time, {
        req(interactions_rv$data)
        interactions_rv$data
      })

      output$plot_sleep <- renderPlot({
        ggplot(data = sleep_data(), aes(x = t, y = asleep)) +
          ggetho::geom_pop_etho() +
          ggetho::scale_x_hours() +
          ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
          facet_wrap("id")
      })

      output$plot_interactions <- renderPlot({
        ggplot(data = interactions_data(), aes(x = t, y = interactions)) +
          ggetho::geom_pop_etho() +
          ggetho::scale_x_hours() +
          ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
          facet_wrap("id")
      })
    }
  )
}