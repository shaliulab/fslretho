
#' @importFrom behavr meta
sqliteDBZIPServer <- function(id, input_rv, monitor) {

  moduleServer(
    id,
    function(input, output, session) {

      output$button <- downloadHandler(

        filename = function() {
          req(input_rv$name)
          tempfile(pattern = paste0(input_rv$name, "sqliteDB"), fileext = ".zip")
        },
        content = function(file) {
          req(monitor() == "ethoscope")
          req(input_rv$data)
          dbfiles <- unique(behavr::meta(input_rv$data)$file_info)
          zip_database(file, dbfiles)
        }
      )
    }
  )
}


sqliteDBZIPUI <- function(id) {

  ns <- NS(id)
  downloadButton(ns("button"), "DB .zip")
}

