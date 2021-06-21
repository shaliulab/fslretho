coerce_columns <- function(data) {

  data$region_id <- as.factor(data$region_id)
  data$datetime <- as.factor(data$region_id)
  data$fly_count <- as.factor(data$fly_count)
  data
}

#' @importFrom esquisse esquisse_server
esquisseModuleServer <- function(id, input_rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      esquisse_rv <- reactiveValues(
        data = NULL, name = NULL, time = NULL
      )

      observeEvent(c(input_rv$time, input_rv$data), {
        esquisse_rv$data <- coerce_columns(input_rv$data)
        esquisse_rv$name <- input_rv$name
        esquisse_rv$time <- input_rv$time
      })

      output_rv <- esquisse::esquisse_server(
        "esquisse",
        data_rv = esquisse_rv,
        data_modal = FALSE,
        ...
      )

      observeEvent(output_rv$time, {
        output_rv$name <- input_rv$name
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}

#' @importFrom esquisse esquisse_ui esquisseContainer
#'
esquisseModuleUI <- function(id, ...) {

  ns <- NS(id)

  esquisse::esquisse_ui(
    ns("esquisse"),
    header = FALSE,
    container = esquisse::esquisseContainer(fixed = FALSE, height = "700px"),
    controls = c("labs", "parameters", "appearance", "filters", "code"),
    insert_code = FALSE,
    ...
  )
}