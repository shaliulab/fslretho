
#' @importFrom behavr meta
sqliteDBZIPServer <- function(id, input_rv, monitor) {

  moduleServer(
    id,
    function(input, output, session) {

      output$button <- downloadHandler(

        filename = function() {
          req(input_rv$name)
          tmp_file <- tempfile(pattern = paste0(input_rv$name, "sqliteDB"), fileext = ".zip")
          message(tmp_file)
          tmp_file
        },
        content = function(file) {
          req(monitor() == "ethoscope")
          req(input_rv$data)
          dbfiles <- unique(behavr::meta(input_rv$data)$file_info)
          message(dbfiles)
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

