#' Statistics computed for each fly over the whole (filtered) experiment
#' @noRd
STATISTICS <- list(mean = mean, std = stats::sd, max = max, min = min, median = stats::median)

#' Summarise each variable of a binned dataset, per fly
#' @param data data.table with an id column (one row per fly and bin)
#' @param meta metadata, joined to the result so it can be used in the plots
#' @return data.table with one row per fly (and interval), and columns like mean-asleep
#' @noRd
summarise_flies <- function(data, meta) {
  data <- data.table::as.data.table(data)
  vars <- intersect(c(Y_VARS, "p_wake", "p_doze"), names(data))
  by <- intersect(c("id", "interval"), names(data))
  out <- data[, {
    res <- list()
    for (v in vars) for (s in names(STATISTICS)) res[[paste0(s, "-", v)]] <- STATISTICS[[s]](stats::na.omit(get(v)))
    res
  }, by = by]

  meta <- data.table::as.data.table(meta)
  meta <- meta[, !sapply(meta, is.list), with = FALSE]
  meta[out, on = "id"]
}

#' @param input_rv dataset to summarise (as filtered in the plot)
#' @param meta_rv dataset whose metadata is added to the summary
#' @noRd
summaryStatisticServer <- function(id, input_rv, meta_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output_rv <- dataset_rv()
      observeEvent(input_rv$time, {
        data <- req(input_rv$data)
        meta <- behavr::meta(req(meta_rv$data))
        safely(context = "summarising the data", {
          set_dataset(output_rv, summarise_flies(data, meta), input_rv$name)
        })
      }, ignoreInit = TRUE)
      output_rv
    }
  )
}
