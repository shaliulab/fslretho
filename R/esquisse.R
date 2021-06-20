#' @importFrom esquisse esquisse_server
#'
esquisseModuleServer <- function(id, input_rv, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- esquisse::esquisse_server(
        "esquisse",
        data_rv = input_rv,
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