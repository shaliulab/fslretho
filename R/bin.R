#' Functions available to summarise each time bin
#' (P(Wake) and P(Doze) are not summary functions: see transition_probabilities)
#' @noRd
SUMMARY_FUNCTIONS <- list(mean = mean, median = stats::median, max = max, min = min)

#' Probability of leaving a state, as defined in Wiggin et al. 2020 (PNAS 117:10024)
#'
#' Every minute in state `from` that is followed by the next minute is an opportunity (denominator);
#' the numerator counts those followed by a minute in the other state.
#' A minute whose next minute is missing (the last one, or one before a gap in the data,
#' e.g. minutes removed by curation) is not an opportunity: there is no transition to see.
#' @param active logical, one value per minute: did the fly move in that minute?
#' @param from the state left: FALSE (inactive) for P(Wake), TRUE (active) for P(Doze)
#' @param t start of each minute (seconds), sorted. By default the minutes are consecutive.
#' @return list with p (NA if there are no opportunities) and n, the number of opportunities
#' @noRd
leave_probability <- function(active, from, t = 60 * seq_along(active)) {
  n <- length(active)
  if (n < 2) return(list(p = NA_real_, n = 0L))
  opportunities <- active[-n] == from & diff(t) == 60
  n_opp <- sum(opportunities)
  list(p = if (n_opp == 0) NA_real_ else sum(opportunities & active[-1] != from) / n_opp, n = n_opp)
}

#' P(Wake) and P(Doze) of each fly in each time bin
#'
#' Computed on 1 minute activity bins, as in Wiggin et al. 2020: a minute is active if the fly
#' moved in it (any moving window of an ethoscope; any beam cross of a DAM monitor).
#' * p_wake: probability that an inactive fly becomes active in the next minute
#' * p_doze: probability that an active fly becomes inactive in the next minute
#' @param d data.table with id, interval, t and moving
#' @param bin_length bin length in seconds
#' @param min_minutes bins where the fly spent fewer minutes in the starting state get NA,
#' because one or two transitions give extreme values (e.g. 1 out of 1). 0 keeps every value.
#' @return data.table with id, interval, t, p_wake, p_doze, n_inactive and n_active
#' @noRd
transition_probabilities <- function(d, bin_length, min_minutes = 5) {
  minutes <- d[, .(active = any(moving)), by = .(id, interval, minute = floor(t / 60) * 60)]
  data.table::setorderv(minutes, c("id", "interval", "minute"))
  out <- minutes[, {
    wake <- leave_probability(active, FALSE, minute)
    doze <- leave_probability(active, TRUE, minute)
    list(p_wake = wake$p, p_doze = doze$p, n_inactive = wake$n, n_active = doze$n)
  }, by = .(id, interval, t = floor(minute / bin_length) * bin_length)]
  out[n_inactive < min_minutes, p_wake := NA_real_]
  out[n_active < min_minutes, p_doze := NA_real_]
  out
}

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
#' * p_wake and p_doze are added, see transition_probabilities
#' * day (1 for ZT 0-24, 2 for ZT 24-48...) and phase (L for ZT 0-12, 24-36...; D for ZT 12-24, 36-48...)
#'   are added to filter the plots
#' @param data behavr table
#' @param minutes bin length in minutes
#' @param FUN summary function
#' @param min_transition_minutes see transition_probabilities
#' @return behavr table, one row per fly, interval and bin. t is the start of the bin.
#' @noRd
bin_dataset <- function(data, minutes, FUN, min_transition_minutes = 5) {
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
  if ("moving" %in% names(d)) {
    out <- transition_probabilities(d, bin_length, min_transition_minutes)[out, on = c("id", "interval", "t")]
  }
  add_day_phase(out)
  data.table::setkeyv(out, "id")
  behavr::setmeta(out, behavr::meta(data))
  out
}

#' Add the day (1 for ZT 0-24, 2 for ZT 24-48...) and the phase (L for ZT 0-12, 24-36...;
#' D for ZT 12-24, 36-48...) of each row, from t (seconds since ZT0)
#' @noRd
add_day_phase <- function(d) {
  d[, day := as.integer(floor(t / 86400) + 1)]
  d[, phase := ifelse((t %% 86400) < 43200, "L", "D")]
  d
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
    selectizeInput(ns("summary_FUN"), label = "Summary function", choices = names(SUMMARY_FUNCTIONS), selected = "mean"),
    numericInput(ns("min_transition_minutes"), label = "Minimum minutes for P(Wake) / P(Doze)", value = 5, min = 0, max = 60, step = 1),
    helpText("Bins where the fly was inactive (active) for fewer minutes get no P(Wake) (P(Doze)). 0 keeps all values.")
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

      observeEvent(c(input_rv$time, input$summary_FUN, input$summary_time_window, input$min_transition_minutes), {
        data <- req(input_rv$data)
        minutes <- if (is.null(summary_time_window)) req(input$summary_time_window) else summary_time_window
        fun_name <- if (is.null(summary_FUN)) req(input$summary_FUN) else summary_FUN

        safely(context = "binning the data", {
          if (!is.null(preproc_FUN)) data <- apply_by_interval(data, preproc_FUN)
          min_minutes <- if (is.null(input$min_transition_minutes) || is.na(input$min_transition_minutes)) 5 else input$min_transition_minutes
          set_dataset(output_rv, bin_dataset(data, minutes, SUMMARY_FUNCTIONS[[fun_name]], min_minutes), input_rv$name)
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
