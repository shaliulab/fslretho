conf <- FSLRethoConfiguration$new()
NCORES <- conf$content$scopr$ncores
CACHE <- conf$content$scopr$folders$cache$path
VERBOSE <- TRUE
TESTING <- conf$content$testing
DEBUG <- conf$content$debug
MAX_POINTS <- Inf


get_x_axis <- function(gg) {

  scales <- gg$scales$scales
  index_x_axis <- lapply(scales, function(s) {"x" %in% s$aesthetics}) %>% unlist %>% which
  x_axis <- scales[[index_x_axis]]
  return(x_axis)
}

#' @import ggplot2
#' @importFrom ggetho scale_x_hours geom_ld_annotations
#' @importFrom htmltools tagAppendAttributes
rawPlotsServer <- function(id, sleep_data, interactions_data) {

  moduleServer(
    id,
    function(input, output, session) {

      theme_set(theme_bw(base_size = 25))
      output_rv <- reactiveValues(
        data = NULL, name = NULL, time = NULL
      )

      output$animal_id_ui <- renderUI({
         #
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
        #
        metadata$file_info <- lapply(metadata$file_info, function(x) list(path=x, filename=basename(x)))
        d <- load_ethoscope(metadata, cache = CACHE, reference_hour = NA)
        d <- d[t > input$time_range[1] * 3600 & t < input$time_range[2] * 3600,]
        # we still want raw data resolution, i.e. annotation
        # for every raw data point
        # therefore the time_window_length must be very small
        sd_annot <- as.data.table(sd_inprogress_annotation(d, time_window_length = 0.1))
        d <- behavr::rejoin(d)

        setkey(d, id, t)
        setkey(sd_annot, id, t)
        d <- d[sd_annot, on=c("t"), roll=T]
        d <- as.data.table(d)
        d <- d[rowSums(is.na(d)) == 0, ]

        d
        row_indices <- seq(1, nrow(d), by=input$downsample)
        d <- d[row_indices,]

        if (nrow(d) > MAX_POINTS) {
          row_indices <- seq(from = 1, to = nrow(d), length.out = MAX_POINTS)
          d <- d[row_indices,]
          d
        }

        if (input$sd_only) {
          before_filter <- nrow(d)
          d <- d[sd_on == TRUE,]
          if (before_filter > 0 & nrow(d) == 0) {
            validate(need(FALSE, "No data under SD"))
          }
        }

        #

        output_rv$raw$data <- d
        output_rv$raw$name <- sleep_data$name
        output_rv$raw$time <- Sys.time()
        d
      }, ignoreInit = TRUE)


      sleep_dataset <- eventReactive(input$button, {
        req(sleep_data$data)
        req(animal_id())
        req(raw_dataset())
        d <- behavr::rejoin(sleep_data$data)[id == animal_id() & t > min(raw_dataset()$t) & t < max(raw_dataset()$t)]
        d
      })

      interactions_dataset <- eventReactive(input$button, {

        req(interactions_data$data)
        req(animal_id())
        req(raw_dataset())
        d <- behavr::rejoin(interactions_data$data)[id == animal_id() & t > min(raw_dataset()$t) & t < max(raw_dataset()$t)]
        d
      })


      plot_x <- reactive({

        validate(need(nrow(raw_dataset()) > 0, "No data available"))
        x_limits <- c(
          floor(min(raw_dataset()$t) / 3600) * 3600,
          ceiling(max(raw_dataset()$t) / 3600) * 3600
        )
        ggplot(data = raw_dataset(), aes(x = t, y = x)) + ggplot2::geom_point() +
          ggetho::scale_x_hours(limits = x_limits) + ggetho::geom_ld_annotations(color=NA, height=1, alpha=0.2) +
          facet_wrap("id")
      })

      output$plot_x <- renderPlot({
        plot_x()
      })

      output$plot_y <- renderPlot({

        x_axis <- get_x_axis(plot_x())
        validate(need(nrow(raw_dataset()) > 0, "No data available"))
        ggplot(data = raw_dataset(), aes(x = t, y = y)) + ggplot2::geom_point() +
          x_axis + ggetho::geom_ld_annotations(color=NA, height=1, alpha=0.2) +
          facet_wrap("id")
      })

      output$plot_sleep <- renderPlot({

        x_axis <- get_x_axis(plot_x())
        ggplot(data = sleep_dataset(), aes(x = t, y = asleep)) +
          ggetho::geom_pop_etho() +
          x_axis +
          ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
          facet_wrap("id")
      })

      output$plot_interactions <- renderPlot({

        x_axis <- get_x_axis(plot_x())

        ggplot(data = interactions_dataset(), aes(x = t, y = interactions)) +
          ggetho::geom_pop_etho() +
          x_axis +
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
      numericInput(ns("downsample"), label = "Downsample", value = 100, min = 1, max = 1000, step = 1),
      uiOutput(ns("animal_id_ui")),
      sliderInput(ns("time_range"), label = "Time range", min = 0, max = behavr::days(7) / 3600, step = 1, value = c(0, behavr::days(7) / 3600), ) %>%
        htmltools::tagAppendAttributes(., style='width: 900px; max-width: 900px')
    ),
    plotOutput(ns("plot_x")),
    plotOutput(ns("plot_y")),
    plotOutput(ns("plot_sleep")),
    plotOutput(ns("plot_interactions"))
  )
}