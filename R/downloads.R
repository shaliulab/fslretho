downloadServerUI <- function(id, label) {

  ns <- NS(id)

  downloadButton(ns("button"), label = label, icon = icon("download"))
}

downloadServer <- function(id, input_rv, dataset_name, monitor = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      output$button <- downloadHandler(
        filename = function() {
          has_csv <- FALSE
          if (grepl(".csv", dataset_name)) has_csv <- T
          if (has_csv) dataset_name <- unlist(strsplit(dataset_name, split = "\\."))[1]
          dataset_name <- paste0(dataset_name, "-", id, ".csv")
        },
        content = function(file) {
          if (is.null(monitor))
            data <- input_rv$data
          else
            data <- input_rv[[monitor()]]$data
          data.table::fwrite(data, file)
        }
      )
    }
  )
}