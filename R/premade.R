premadePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot_sleep"), height = "600px"),
    plotOutput(ns("plot_interactions"), height = "600px")
  )
}

#' Sleep and SD interactions of every fly, without any configuration
#' @noRd
premadePlotsServer <- function(id, sleep_rv, interactions_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_sleep <- renderPlot({
        raw_plot(behavr::rejoin(req(sleep_rv$data)), "asleep", NULL, ggetho::geom_pop_etho())
      })
      # SD interactions for ethoscopes; beam crosses for DAM monitors
      output$plot_interactions <- renderPlot({
        data <- behavr::rejoin(req(interactions_rv$data))
        y <- intersect(c("interactions", "activity"), names(data))
        req(length(y) > 0)
        raw_plot(data, y[1], NULL, ggetho::geom_pop_etho())
      })
    }
  )
}
