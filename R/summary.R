STATISTICS <- list(
  mean = base::mean,
  std = stats::sd,
  max = base::max,
  min = base::min,
  median = stats::median
)

for (i in 1:length(STATISTICS)) {
  scope <- function(i) {
    # print(i)
    attr(STATISTICS[[i]], "name") <<- function() {names(STATISTICS)[i]}
  }
  scope(i)
}


summaryStatisticUI <- function(id, var) {

  ns <- NS(id)

  tagList(
    textInput(ns("y"), "Summary over time", value=var)
    # selectizeInput(ns("stats"), "Summary statistic", multiple = TRUE, choices = names(STATISTICS), selected = "mean")
  )

}

#' @param data data.table
#' @param col column in this data table
#' @param func Function to summarise the data with
summary_wrapper <- function(data, col, func) {

  # statistic <- tryCatch({
    statistic <- attr(func, "name")()
  # }, error = function(e) {browser()})
  data$target__ <- data[[col]]
  sum_data <- data[, .SD[, func(target__),], by=eval(data.table::key(data))]
  colnames(sum_data)[colnames(sum_data) == "V1"] <- paste0(statistic, "-", col)
  return(sum_data)
}


summaryStatisticServer <- function(id, input_rv, FUN) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

      # observing everything can break the
      observeEvent(input_rv$time, {
        req(input$y)
        req(input_rv$data)
        # TODO Warn the user if this is not true!
        req(input$y %in% colnames(input_rv$data))
        FUN <- STATISTICS
        summarised_data <- lapply(FUN, function(func) summary_wrapper(input_rv$data, input$y, func))
        if (length(FUN) > 1)
          summarised_data <- Reduce(merge, summarised_data)
        else
          summarised_data <- summarised_data[[1]]

        output_rv$data <- summarised_data
        output_rv$name <- input_rv$name
        output_rv$time <- input_rv$time
      }, ignoreInit = TRUE)

      return(output_rv)
    }
  )
}
