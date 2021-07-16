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

        data <- req(input_rv$data)
        if ("behavr" %in% class(data)) data <- behavr::rejoin(data)
        esquisse_rv$data <- coerce_columns(data)
        esquisse_rv$name <- input_rv$name
        esquisse_rv$time <- Sys.time()
      })

      output_rv <- esquisse::esquisse_server(
        "esquisse",
        default_aes = c("fill", "color", "size", "group", "facet", "facet_row", "facet_col", "linetype"),
        data_rv = esquisse_rv,
        data_modal = FALSE,
        ...
      )


      observeEvent(output_rv$time, {
        output_rv$data
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


#' @import dplyr
esquisseReplayServer <- function(id, data_rv, esquisse_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(data =  NULL, name = NULL , time = NULL)

      observeEvent(c(data_rv$time, esquisse_rv$time), {
        req(data_rv$data)
        req(esquisse_rv$data)
        # data  <- list(req(data_rv$data))
        # names(data) <- data_rv$name
        # filtered_data <- rlang::eval_tidy(expr = req(esquisse_rv$code_filters), data = data)
        output_rv$data <- behavr::unjoin(data_rv$data, esquisse_rv$data)
        output_rv$name <- data_rv$name
        output_rv$time <- Sys.time()
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}