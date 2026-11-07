#' One value per fly, shown as bars below the Sleep and Bout plots

#' Periods the per-fly values can be computed over
#' @noRd
SUMMARY_PERIODS <- c(
  "Whole period shown (total)" = "total",
  "Average per day" = "per_day",
  "Light phase (ZT 0-12), per day" = "L",
  "Dark phase (ZT 12-24), per day" = "D"
)

#' Sort the flies by date, machine (or DAM monitor file) and region
#' @return the ids, in order
#' @noRd
sorted_flies <- function(meta) {
  meta <- data.table::as.data.table(meta)
  date <- if ("datetime" %in% names(meta)) as.character(meta$datetime)
          else if ("start_datetime" %in% names(meta)) as.character(meta$start_datetime)
          else if ("date" %in% names(meta)) as.character(meta$date) else ""
  machine <- if ("machine_name" %in% names(meta)) as.character(meta$machine_name)
             else if ("file_info" %in% names(meta)) basename(as.character(unlist(meta$file_info))) else ""
  region <- if ("region_id" %in% names(meta)) as.integer(meta$region_id) else 0L
  as.character(meta$id[order(date, machine, region)])
}

#' Label of the value plotted, e.g. "Sleep per day (min)"
#' @noRd
summary_label <- function(var, period) {
  name <- switch(var, asleep = "Sleep", duration = "Bout duration", p_wake = "P(Wake)", p_doze = "P(Doze)", var)
  if (var == "asleep") {
    paste0(name, switch(period, total = "", per_day = " per day", L = " in the light phase, per day", D = " in the dark phase, per day"), " (min)")
  } else {
    paste0(name, if (var == "duration") " (min)", switch(period, total = "", per_day = "", L = ", light phase", D = ", dark phase"))
  }
}

#' Value of a variable for every fly over a period
#'
#' @param data binned data (one row per fly and bin, t in seconds since ZT0), as filtered in the plot above
#' @param meta metadata of the flies, added to the result
#' @param var variable: for asleep, the minutes of sleep; for duration (bouts), the mean bout length in minutes;
#' otherwise the mean of the variable
#' @param period one of SUMMARY_PERIODS
#' @return data.table with the metadata, value, and hours (amount of data used for each fly)
#' @noRd
summarise_flies <- function(data, meta, var, period = "total") {
  d <- data.table::as.data.table(data)
  d <- d[!is.na(get(var))]
  if (nrow(d) == 0) return(NULL)
  bin <- stats::median(diff(sort(unique(d$t))))
  if (!is.finite(bin) || bin <= 0) bin <- 60
  if (period %in% c("L", "D")) d <- d[((t %% 86400) < 43200) == (period == "L")]

  out <- d[, .(
    hours = .N * bin / 3600,
    value = if (var == "asleep") sum(get(var)) * bin / 60
            else if (var == "duration") mean(get(var)) / 60
            else mean(get(var))
  ), by = id]
  # minutes of sleep per day: divide by the number of (light / dark) days in the data of each fly
  if (var == "asleep" && period != "total") {
    out[, value := value / (hours / if (period == "per_day") 24 else 12)]
  }

  meta <- data.table::as.data.table(meta)
  meta <- meta[, !sapply(meta, is.list), with = FALSE]
  out <- meta[out, on = "id"]
  out[, id := factor(as.character(id), levels = intersect(sorted_flies(meta), as.character(id)))]
  out[order(id)]
}

flySummaryUI <- function(id) {
  ns <- NS(id)
  shinydashboard::box(
    width = NULL, title = "One value per fly",
    fluidRow(
      column(3, uiOutput(ns("var_ui"))),
      column(4, selectInput(ns("period"), "Over", choices = SUMMARY_PERIODS, width = "100%")),
      column(3, uiOutput(ns("fill_ui")))
    ),
    div(
      style = "position: relative;",
      plotOutput(ns("plot"), height = "400px", hover = hoverOpts(ns("hover"), delay = 80, delayType = "throttle")),
      uiOutput(ns("tooltip"))
    ),
    helpText("Computed on the data shown in the plot above (with its filters). Flies are sorted by date, machine and ROI: hover a bar to see which fly it is.")
  )
}

#' @param plot_rv dataset shown in the plot above (filtered binned data)
#' @param meta_rv the loaded dataset, for the metadata
#' @param vars variables offered; the first one is selected
#' @return dataset with the per-fly values
#' @noRd
flySummaryServer <- function(id, plot_rv, meta_rv, vars) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()

      output$var_ui <- renderUI({
        available <- intersect(vars, names(req(plot_rv$data)))
        selectInput(session$ns("var"), "Variable", choices = available, selected = available[1], width = "100%")
      })

      output$fill_ui <- renderUI({
        meta <- behavr::meta(req(meta_rv$data))
        choices <- setdiff(names(meta)[!sapply(meta, is.list)], c("id", "file_info"))
        selectInput(session$ns("fill"), "Colour by", choices = c("(none)" = "", choices),
                    selected = intersect(c("genotype", "condition", "treatment"), choices)[1], width = "100%")
      })

      summary <- reactive({
        req(plot_rv$data, meta_rv$data, input$var, input$period)
        safely(context = "summarising the flies", summarise_flies(plot_rv$data, behavr::meta(meta_rv$data), input$var, input$period))
      })

      observeEvent(summary(), set_dataset(output_rv, summary(), plot_rv$name))

      output$plot <- renderPlot({
        s <- req(summary())
        fill <- if (isTruthy(input$fill) && input$fill %in% names(s)) input$fill else NULL
        gg <- ggplot2::ggplot(s, ggplot2::aes(x = id, y = value)) +
          (if (is.null(fill)) ggplot2::geom_col(fill = "#3c8dbc", width = 0.85)
           else ggplot2::geom_col(ggplot2::aes(fill = factor(.data[[fill]])), width = 0.85)) +
          ggplot2::labs(x = sprintf("Flies (%d), sorted by date, machine and ROI", nrow(s)),
                        y = summary_label(input$var, input$period), fill = fill) +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                         panel.grid.major.x = ggplot2::element_blank())
        gg
      })

      output$tooltip <- renderUI({
        hover <- input$hover
        s <- summary()
        if (is.null(hover) || is.null(s)) return(NULL)
        i <- round(hover$x)
        if (i < 1 || i > nrow(s) || is.null(hover$y) || hover$y > max(s$value[i], 0) || hover$y < min(s$value[i], 0)) return(NULL)
        fly <- s[i]
        rows <- list(
          c("Fly", as.character(fly$id)),
          if ("machine_name" %in% names(fly)) c("Machine", fly$machine_name)
          else if ("experiment_id" %in% names(fly)) c("Monitor", sub(".*\\|", "", fly$experiment_id)),
          if ("region_id" %in% names(fly)) c("ROI", fly$region_id),
          if (isTruthy(input$fill) && input$fill %in% names(fly)) c(input$fill, as.character(fly[[input$fill]])),
          c(summary_label(input$var, input$period), format(signif(fly$value, 3))),
          c("Hours of data", format(round(fly$hours, 1)))
        )
        rows <- rows[!sapply(rows, is.null)]
        div(
          class = "fsl-tooltip",
          style = sprintf("left: %dpx; top: %dpx;", round(hover$coords_css$x) + 15, round(hover$coords_css$y) + 10),
          lapply(rows, function(r) div(tags$b(paste0(r[1], ": ")), r[2]))
        )
      })

      output_rv
    }
  )
}
