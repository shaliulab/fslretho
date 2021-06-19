downloadServerUI <- function(id, label) {

  ns <- NS(id)

  downloadButton(ns("button"), label = label, icon = icon("download"))
}

downloadServer <- function(id, input_rv, dataset_name) {

  moduleServer(
    id,
    function(input, output, session) {

      output$button <- downloadHandler(
        filename = function() {
          "fslretho_dataset.csv"
        },
        content = function(file) {
          data.table::fwrite(input_rv$data, file)
        }
      )
    }
  )
}