MAX_POINTS <- 5000

#' @import ggplot2
#' @importFrom ggetho scale_x_hours geom_ld_annotations
rawPlotsServer <- function(id, raw_data, scored_data, sleep_data, monitor) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        raw = reactiveValues(data = NULL, name = NULL, time = NULL),
        scored = reactiveValues(data = NULL, name = NULL, time = NULL)
      )


      input_raw_rv <- reactive({
        req(monitor())
        raw_data[[monitor()]]
      })


      output$animal_id_ui <- renderUI({
        req(monitor())
        req(raw_data[[monitor()]]$data)
        selectizeInput(
          session$ns("animal_id"), label = "Animal id",
          choices = behavr::meta(raw_data[[monitor()]]$data)$id
        )
      })


      animal_id <- reactive(
        input$animal_id
      )

      raw_dataset <- eventReactive(input$button, {
        req(input_raw_rv()$data)
        req(animal_id())
        d <- input_raw_rv()$data[id == animal_id(),]

        if (nrow(d) > MAX_POINTS) {
          row_indices <- seq(from = 1, to = nrow(d), length.out = MAX_POINTS)
          d <- d[row_indices,]
          d
        }

        output_rv$raw$data <- d
        output_rv$raw$name <- input_raw_rv()$data$data$name
        output_rv$raw$time <- input_raw_rv()$data$data$time
        d
      }, ignoreInit = TRUE)

      scored_dataset <- eventReactive(input$button, {
        req(scored_data[[monitor()]]$data)
        req(animal_id())
        d <- scored_data[[monitor()]]$data[id == animal_id(),]

        if (nrow(d) > MAX_POINTS) {
          row_indices <- seq(from = 1, to = nrow(d), length.out = MAX_POINTS)
          d <- d[row_indices,]
          d
        }

        output_rv$scored$data <- d
        output_rv$scored$name <- scored_data[[monitor()]]$data$name
        output_rv$scored$time <- scored_data[[monitor()]]$data$time
        d
      }, ignoreInit = TRUE)


      sleep_dataset <- eventReactive(input$button, {
        req(sleep_data$data)
        req(animal_id())

        d <- sleep_data$data[id == animal_id()]
        d
      })

      output$plot_raw <- renderPlot({
        ggplot(data = raw_dataset(), aes(x = t, y = x)) + ggplot2::geom_point() +
          ggetho::scale_x_hours() + ggetho::geom_ld_annotations(color=NA, height=1, alpha=0.2) +
          facet_wrap("id")
      })

      output$plot_scored <- renderPlot({
        # TODO
      })

      output$plot_sleep <- renderPlot({
        ggplot(data = sleep_dataset(), aes(x = t, y = asleep)) +
          ggetho::geom_pop_etho() +
          ggetho::scale_x_hours() +
          ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
          facet_wrap("id")
      })


      return(output_rv)
    }
  )
}

rawPlotsUI <- function(id) {

  ns <- NS(id)

  tagList(
    actionButton(ns("button"), "Plot"),
    uiOutput(ns("animal_id_ui")),
    plotOutput(ns("plot_raw")),
    plotOutput(ns("plot_scored")),
    plotOutput(ns("plot_sleep"))
  )
}