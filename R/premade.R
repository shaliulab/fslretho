#' Premade plots without any user tweaking
#'
#' @param data reactiveValues with a data slot containing an ethoscope behavr table
#' @importFrom cowplot plot_grid
sleepInteractionsServer <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {

      analyse_sleep_00 <- reactiveVal(NULL)

      output$sleep_interactions_plot <- renderPlot({
        input$refresh_analyseSleep_00
        analyse_sleep_00()
      })

      observeEvent(data$data, {
        req(data$data$t)
        req(data$data$asleep)
        req(data$data[, meta = T]$region_id)
        data <- behavr::rejoin(data$data)

        sleep_trace <- ggplot2::ggplot(data = data, ggplot2::aes(x = t, y = asleep)) +
          ggetho::stat_pop_etho() +
          ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
          ggplot2::facet_grid(region_id ~ .)

        if (isTruthy(data$data$interactions)) {
          interactions_trace <- ggplot2::ggplot(data = data, ggplot2::aes(x = t, y = interactions)) +
            ggetho::stat_pop_etho() +
            ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA)  +
            ggplot2::facet_grid(region_id ~ .) +
            ggetho::scale_x_hours()

          output_plot <- cowplot::plot_grid(sleep_trace, interactions_trace, ncol = 2)
        } else {
          output_plot <- sleep_trace
        }

        analyse_sleep_00(output_plot)

      }, ignoreNULL = TRUE)
    }
  )
}