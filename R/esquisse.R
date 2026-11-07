#' Treat identifiers as categories in the plots
#' @noRd
coerce_columns <- function(data) {
  for (col in intersect(c("region_id", "datetime", "fly_count"), names(data))) {
    data.table::set(data, j = col, value = as.factor(data[[col]]))
  }
  # esquisse's filters cannot handle Inf (e.g. max_velocity when two frames share a timestamp)
  for (col in names(data)[sapply(data, is.double)]) {
    bad <- which(is.infinite(data[[col]]))
    if (length(bad) > 0) data.table::set(data, i = bad, j = col, value = NA_real_)
  }
  data
}

#' Plot a dataset with esquisse
#' @param ... passed to esquisse::esquisse_server (e.g. the default plot)
#' @return dataset with the data as filtered in esquisse
#' @noRd
esquisseModuleServer <- function(id, input_rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      esquisse_rv <- dataset_rv()

      observeEvent(input_rv$time, {
        data <- req(input_rv$data)
        if (inherits(data, "behavr")) data <- behavr::rejoin(data)
        set_dataset(esquisse_rv, coerce_columns(data.table::as.data.table(data)), input_rv$name)
      })

      output_rv <- esquisse::esquisse_server(
        "esquisse",
        default_aes = c("fill", "color", "size", "group", "facet", "facet_row", "facet_col", "linetype"),
        data_rv = esquisse_rv,
        data_modal = FALSE,
        ...
      )

      observeEvent(output_rv$time, {
        output_rv$name <- input_rv$name
      }, ignoreInit = TRUE)

      output_rv
    }
  )
}

esquisseModuleUI <- function(id) {
  ns <- NS(id)
  esquisse::esquisse_ui(
    ns("esquisse"),
    header = FALSE,
    container = esquisse::esquisseContainer(fixed = FALSE, height = "700px"),
    controls = c("labs", "parameters", "appearance", "filters", "code"),
    insert_code = FALSE
  )
}

#' Apply the filters made in esquisse to the original (behavr) dataset
#' @noRd
esquisseReplayServer <- function(id, data_rv, esquisse_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output_rv <- dataset_rv()
      observeEvent(c(data_rv$time, esquisse_rv$time), {
        req(data_rv$data, esquisse_rv$data)
        safely(context = "applying the plot filters", {
          set_dataset(output_rv, behavr::unjoin(data_rv$data, esquisse_rv$data), data_rv$name)
        })
      }, ignoreInit = TRUE)
      output_rv
    }
  )
}
