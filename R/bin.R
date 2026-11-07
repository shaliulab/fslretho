#' Functions available to summarise each time bin
#' @noRd
SUMMARY_FUNCTIONS <- list(
  mean = mean, median = stats::median, max = max, min = min,
  P_doze = sleepr::p_doze, P_wake = sleepr::p_wake
)

#' Variables that are summarised in each bin, if present in the data
#' @noRd
Y_VARS <- c(
  "asleep", "moving", "interactions", "is_interpolated",
  "microbehavior", "micromovement", "beam_crosses", "max_velocity",
  "duration", "body_movement", "activity"
)

#' Bin a behavr table in time
#'
#' Within each fly, interval (default / SD) and time bin:
#' * the variables in Y_VARS are summarised with FUN (e.g. the fraction of time asleep if FUN = mean)
#' * sd_on becomes TRUE if the SD was running during the whole bin
#' * x (position in the tube) becomes pareto, see pareto_sd
#' @param data behavr table
#' @param minutes bin length in minutes
#' @param FUN summary function
#' @return behavr table, one row per fly, interval and bin. t is the start of the bin.
#' @noRd
bin_dataset <- function(data, minutes, FUN) {
  bin_length <- behavr::mins(minutes)
  d <- data.table::as.data.table(data)
  if (!"interval" %in% names(d)) d[, interval := "default"]

  vars <- intersect(Y_VARS, names(d))
  has_sd <- "sd_on" %in% names(d)
  has_x <- "x" %in% names(d)

  out <- d[,
    c(
      lapply(.SD, FUN),
      if (has_sd) list(sd_on = all(sd_on)),
      if (has_x) list(pareto = pareto_sd(x))
    ),
    by = .(id, interval, t = floor(t / bin_length) * bin_length),
    .SDcols = vars
  ]
  data.table::setkeyv(out, "id")
  behavr::setmeta(out, behavr::meta(data))
  out
}

#' Apply a function to each interval (default / SD) of a behavr table separately
#' @noRd
apply_by_interval <- function(data, FUN) {
  if (!"interval" %in% names(data)) return(FUN(data))
  parts <- lapply(unique(data$interval), function(interv) {
    d <- FUN(data[interval == interv])
    if (!is.null(d)) d[, interval := interv]
    d
  })
  # the same fly appears in every interval: stack the data and attach the metadata once
  out <- data.table::rbindlist(lapply(parts, data.table::as.data.table), use.names = TRUE, fill = TRUE)
  data.table::setkeyv(out, "id")
  behavr::setmeta(out, behavr::meta(data))
  out
}

binDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("summary_time_window"), label = "Bin length (minutes)", value = 30, min = 5, max = 120, step = 5),
    selectizeInput(ns("summary_FUN"), label = "Summary function", choices = names(SUMMARY_FUNCTIONS), selected = "mean")
  )
}

#' Bin a dataset in time
#' @param summary_time_window,summary_FUN if given, they override the user inputs
#' @param preproc_FUN function applied to the data (of each interval) before binning,
#' e.g. bout_analysis
#' @noRd
binDataServer <- function(id, input_rv, summary_time_window = NULL, summary_FUN = NULL, preproc_FUN = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()

      observeEvent(c(input_rv$time, input$summary_FUN, input$summary_time_window), {
        data <- req(input_rv$data)
        minutes <- if (is.null(summary_time_window)) req(input$summary_time_window) else summary_time_window
        fun_name <- if (is.null(summary_FUN)) req(input$summary_FUN) else summary_FUN

        safely(context = "binning the data", {
          if (!is.null(preproc_FUN)) data <- apply_by_interval(data, preproc_FUN)
          set_dataset(output_rv, bin_dataset(data, minutes, SUMMARY_FUNCTIONS[[fun_name]]), input_rv$name)
        })
      })

      output_rv
    }
  )
}

#' Compute sleep bouts
#' @param data behavr table with a column called like `var`
#' @param var name of the binary variable whose bouts are computed
#' @seealso sleepr::bout_analysis
#' @export
bout_analysis <- function(data, var = "asleep") {
  sleepr::bout_analysis_standard(var_name = var, data = data)
}
