conf <- FSLRethoConfiguration$new()
NCORES <- conf$content$scopr$ncores
CACHE <- conf$content$scopr$folders$cache$path
VERBOSE <- TRUE
TESTING <- conf$content$testing
DEBUG <- conf$content$debug
MAX_POINTS <- Inf


#' @import ggplot2
#' @importFrom ggetho scale_x_hours geom_ld_annotations
rawPlotsServer <- function(id, sleep_data) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        data = NULL, name = NULL, time = NULL
      )

      output$animal_id_ui <- renderUI({
         # browser()
        req(sleep_data$data)
        selectizeInput(
          session$ns("animal_id"), label = "Animal id",
          choices = behavr::meta(sleep_data$data)$id
        )
      })


      animal_id <- reactive(
        input$animal_id
      )

      raw_dataset <- eventReactive(input$button, {
        req(animal_id())

        metadata <- behavr::meta(sleep_data$data)
        metadata <- metadata[id == animal_id()]
        # browser()
        metadata$file_info <- lapply(metadata$file_info, function(x) list(path=x, filename=basename(x)))
        d <- load_ethoscope(metadata, cache = CACHE, reference_hour = NA)
        d


        row_indices <- seq(1, nrow(d), by=input$downsample)
        d <- d[row_indices,]

        if (nrow(d) > MAX_POINTS) {
          row_indices <- seq(from = 1, to = nrow(d), length.out = MAX_POINTS)
          d <- d[row_indices,]
          d
        }

        if (input$sd_only) {
          d <- d[isTRUE(sd_on),]
        }

        output_rv$raw$data <- d
        output_rv$raw$name <- sleep_data$name
        output_rv$raw$time <- Sys.time()
        d
      }, ignoreInit = TRUE)


      sleep_dataset <- eventReactive(input$button, {
        req(sleep_data$data)
        req(animal_id())
        d <- behavr::rejoin(sleep_data$data)[id == animal_id()]
        d
      })

      output$plot_raw <- renderPlot({
        ggplot(data = raw_dataset(), aes(x = t, y = x)) + ggplot2::geom_point() +
          ggetho::scale_x_hours() + ggetho::geom_ld_annotations(color=NA, height=1, alpha=0.2) +
          facet_wrap("id")
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
    inputPanel(
      actionButton(ns("button"), "Plot"),
      checkboxInput(ns("sd_only"), "SD only", value = TRUE),
      numericInput(ns("downsample"), label = "Downsample", value = 10, min = 1, max = 100, step = 1),
      uiOutput(ns("animal_id_ui"))
    ),
    plotOutput(ns("plot_raw")),
    plotOutput(ns("plot_sleep"))
  )
}