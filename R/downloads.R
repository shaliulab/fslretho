#' Datasets offered in the "Download datasets" menu: id -> label
#' @noRd
DOWNLOADS <- c(
  `sleep-binned` = "Sleep (binned, as plotted)",
  `sleep-scored` = "Sleep (scored, one row per window)",
  `raw-data` = "Raw data (Raw tab)",
  `bouts` = "Sleep bouts (as plotted)",
  `sleep-summary` = "Sleep summary",
  `bouts-summary` = "Sleep bouts summary",
  `periodogram` = "Periodogram",
  `spectrogram` = "Spectrogram"
)

downloadsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "fsl-downloads",
    downloadButton(ns("dbfiles"), "Raw data files: ethoscope .db / DAM .txt (.zip)", class = "btn-block"),
    lapply(names(DOWNLOADS), function(name) downloadButton(ns(name), DOWNLOADS[[name]], class = "btn-block"))
  )
}

#' Descriptive column name of a binned variable, e.g. fraction_of_time_asleep
#' @param var variable
#' @param fun summary function used to bin it (mean, median...)
#' @noRd
binned_column_name <- function(var, fun = "mean") {
  if (var %in% c("p_wake", "p_doze")) return(var)
  if (identical(fun, "mean")) {
    nice <- c(asleep = "fraction_of_time_asleep", moving = "fraction_of_time_moving",
              duration = "mean_bout_duration_s", activity = "mean_beam_crosses_per_minute",
              interactions = "mean_sd_interactions_per_window")
    if (var %in% names(nice)) return(nice[[var]])
  }
  paste0(fun, "_", var)
}

#' Turn a label such as "Sleep per day (min)" into a column name such as sleep_per_day_min
#' @noRd
column_name <- function(label) {
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(label)))
}

#' The columns of each download: id, t for time series, and the value with a descriptive name
#' @param name one of names(DOWNLOADS)
#' @param y variable on the Y axis of the plot the dataset comes from (binned data)
#' @param fun summary function of the binning
#' @return data.table
#' @noRd
export_table <- function(name, data, y = NULL, fun = "mean") {
  # a copy: the dataset is still used by the app
  d <- data.table::copy(data.table::as.data.table(data))
  keep <- function(cols, new_names = cols) {
    names(new_names) <- cols
    present <- intersect(cols, names(d))
    out <- d[, present, with = FALSE]
    data.table::setnames(out, present, unname(new_names[present]))
    out
  }
  switch(
    name,
    `sleep-binned` = , `bouts` = {
      if (is.null(y) || !y %in% names(d)) y <- if (name == "bouts") "duration" else "asleep"
      keep(c("id", "t", y), c("id", "t", binned_column_name(y, fun)))
    },
    `sleep-scored` = keep(c("id", "t", "asleep")),
    `raw-data` = keep(c("id", "t", "x", "y"), c("id", "t", "x_position", "y_position")),
    `sleep-summary` = , `bouts-summary` = keep(c("id", "value"), c("id", column_name(attr(data, "value_label") %||% "value"))),
    `periodogram` = {
      d[, period_h := period / 3600]
      keep(c("id", "period_h", "power"))
    },
    `spectrogram` = {
      d[, period_h := period / 3600]
      keep(c("id", "t", "period_h", "power"))
    }
  )
}

#' @param datasets named list of datasets (reactiveValues with data and name),
#' with the names of DOWNLOADS
#' @param loaded the loaded dataset, whose .db files can be downloaded
#' @param plot_y named list of reactives: variable on the Y axis of the plot of a dataset
#' @param summary_fun named list of reactives: summary function used to bin a dataset
#' @noRd
downloadsServer <- function(id, datasets, loaded, plot_y = list(), summary_fun = list()) {
  moduleServer(
    id,
    function(input, output, session) {

      # Read the dataset when the user clicks, not when the app starts
      nothing_to_download <- function(label) {
        showNotification(
          sprintf("'%s' is not available yet. Load data and open the corresponding tab first.", label),
          type = "warning"
        )
        req(FALSE)
      }

      lapply(names(DOWNLOADS), function(name) {
        rv <- datasets[[name]]
        output[[name]] <- downloadHandler(
          filename = function() paste0(dataset_basename(rv$name), "-", name, ".csv"),
          content = function(file) {
            if (is.null(rv$data) || nrow(rv$data) == 0) nothing_to_download(DOWNLOADS[[name]])
            y <- if (!is.null(plot_y[[name]])) plot_y[[name]]()
            fun <- if (!is.null(summary_fun[[name]])) summary_fun[[name]]() else "mean"
            data.table::fwrite(export_table(name, rv$data, y, if (is.null(fun)) "mean" else fun), file)
          }
        )
      })

      output$dbfiles <- downloadHandler(
        filename = function() paste0(dataset_basename(loaded$name), "-rawfiles.zip"),
        content = function(file) {
          if (is.null(loaded$data)) nothing_to_download("Raw data files")
          dbfiles <- unique(as.character(unlist(behavr::meta(loaded$data)$file_info)))
          id <- showNotification(sprintf("Compressing %d file(s), please wait...", length(dbfiles)), duration = NULL)
          on.exit(removeNotification(id))
          # -j: store the files without their folders
          utils::zip(file, dbfiles, flags = "-j")
        },
        contentType = "application/zip"
      )
    }
  )
}
