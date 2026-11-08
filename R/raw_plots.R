#' Raw tab: the tracking data of a single fly, at full resolution, next to its sleep trace
#'
#' The raw data is loaded again from the .db file (it is not kept in memory)
#' and annotated with whether the SD was running.
#' @noRd
load_raw_fly <- function(meta, time_range, downsample) {
  meta <- data.table::copy(meta)
  # file_info was simplified to a path when the data was loaded
  meta$file_info <- lapply(meta$file_info, function(x) list(path = x, file = basename(x)))
  d <- scopr::load_ethoscope(meta, cache = fslretho_conf()$scopr$folders$cache$path, reference_hour = NA)
  if (is.null(d) || nrow(d) == 0) stop("there is no tracking data for this fly")
  d <- d[t > time_range[1] * 3600 & t < time_range[2] * 3600]
  if (nrow(d) == 0) stop("this fly has no data in the selected time range")

  # annotate every raw data point: the time window must be very small
  sd_annot <- data.table::as.data.table(sd_inprogress_annotation(d, time_window_length = 0.1))
  d <- data.table::as.data.table(behavr::rejoin(d))
  # list columns (file_info) are not needed and break complete.cases
  d[, names(d)[sapply(d, is.list)] := NULL]
  data.table::setkey(d, id, t)
  data.table::setkey(sd_annot, id, t)
  d <- sd_annot[, .(t, sd_on)][d, on = "t", roll = TRUE]
  d <- d[stats::complete.cases(d)]
  d[seq(1, nrow(d), by = downsample)]
}

raw_plot <- function(data, y, x_limits, geom = ggplot2::geom_point()) {
  ggplot2::ggplot(data, ggplot2::aes(x = t, y = .data[[y]])) +
    geom +
    ggetho::scale_x_hours(limits = x_limits) +
    ggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
    ggplot2::facet_wrap("id") +
    ggplot2::theme_bw(base_size = 20)
}

rawPlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, uiOutput(ns("animal_id_ui"))),
      column(4, sliderInput(ns("time_range"), label = "Time range (hours since ZT0)", min = 0, max = 7 * 24, step = 1, value = c(0, 7 * 24), width = "100%")),
      column(2, numericInput(ns("downsample"), label = "Keep 1 point every", value = 100, min = 1, max = 1000, step = 1)),
      column(2, checkboxInput(ns("sd_only"), "SD only", value = TRUE)),
      column(1, actionButton(ns("button"), "Plot", class = "btn-primary"))
    ),
    helpText("Click on any plot to open the snapshot taken closest to that time, in the snapshot viewer."),
    uiOutput(ns("hover_info")),
    lapply(RAW_PLOTS, function(p) {
      div(class = "fsl-clickable", plotOutput(
        ns(p), click = ns(paste0(p, "_click")),
        hover = hoverOpts(ns(paste0(p, "_hover")), delay = 100, delayType = "throttle"), height = "300px"
      ))
    })
  )
}

RAW_PLOTS <- c("plot_x", "plot_y", "plot_sleep", "plot_interactions")

#' @param snapshot_request reactiveVal read by the snapshot viewer (see snapshotViewerServer)
#' @return dataset with the raw data of the plotted fly
#' @noRd
rawPlotsServer <- function(id, sleep_data, interactions_data, snapshot_request = reactiveVal(NULL), parent_session = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()
      fly_meta <- reactiveVal(NULL)

      # click on a plot: show the closest snapshot
      for (p in RAW_PLOTS) local({
        click <- paste0(p, "_click")
        observeEvent(input[[click]], {
          meta <- req(fly_meta())
          snapshot_request(list(
            dbfile = as.character(unlist(meta$file_info))[1], t = input[[click]]$x,
            reference_hour = meta$reference_hour[1], at = Sys.time()
          ))
          if (!is.null(parent_session)) shinydashboard::updateTabItems(parent_session, "tabs", "snapshot")
        })
      })

      output$hover_info <- renderUI({
        hover <- NULL
        for (p in RAW_PLOTS) if (!is.null(input[[paste0(p, "_hover")]])) hover <- input[[paste0(p, "_hover")]]
        if (is.null(hover) || is.null(fly_meta())) return(tags$p(HTML("&nbsp;")))
        tags$p(tags$b(sprintf("ZT %.2f h", hover$x / 3600)), "- click to open the closest snapshot")
      })

      output$animal_id_ui <- renderUI({
        selectizeInput(session$ns("animal_id"), label = "Fly", choices = as.character(behavr::meta(req(sleep_data$data))$id))
      })

      observeEvent(input$button, {
        if (is_dam_dataset(req(sleep_data$data))) {
          showNotification("The Raw tab shows ethoscope tracking data. DAM monitors only record beam crosses: see their activity in the Sleep analysis tab.", type = "warning", duration = 10)
          return()
        }
        meta <- behavr::meta(req(sleep_data$data))
        meta <- meta[id == req(input$animal_id)]
        if (nrow(meta) == 0) {
          showNotification("This fly is not in the loaded dataset anymore. Pick another one.", type = "warning")
          return()
        }
        sd_only <- input$sd_only
        name <- sleep_data$name
        fly_meta(meta)
        in_background(
          "load_raw_fly", list(meta = meta, time_range = input$time_range, downsample = max(1, input$downsample)),
          context = "loading the raw data",
          on_done = function(d) {
            if (sd_only) {
              d <- d[sd_on == TRUE]
              if (nrow(d) == 0) {
                showNotification("This fly has no data during SD. Untick 'SD only' to see all the data.", type = "warning")
                return()
              }
            }
            set_dataset(output_rv, d, name)
          }
        )
      })

      x_limits <- reactive({
        t <- req(output_rv$data)$t
        c(floor(min(t) / 3600), ceiling(max(t) / 3600)) * 3600
      })

      # binned data of the plotted fly, in the plotted time range
      fly_binned <- function(rv) {
        fly <- as.character(req(output_rv$data)$id[1])
        limits <- x_limits()
        d <- data.table::as.data.table(behavr::rejoin(req(rv$data)))
        d[as.character(id) == fly & t >= limits[1] & t <= limits[2]]
      }

      output$plot_x <- renderPlot(raw_plot(output_rv$data, "x", x_limits()))
      output$plot_y <- renderPlot(raw_plot(output_rv$data, "y", x_limits()))
      output$plot_sleep <- renderPlot(raw_plot(fly_binned(sleep_data), "asleep", x_limits(), ggetho::geom_pop_etho()))
      output$plot_interactions <- renderPlot(raw_plot(fly_binned(interactions_data), "interactions", x_limits(), ggetho::geom_pop_etho()))

      output_rv
    }
  )
}
