
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
          tmp_file <- tempfile(fileext = ".zip")
          zip_database(tmp_file, dbfiles)
          wait_max <- 60
          have_waited <- 0
          while (!file.exists(tmp_file) & have_waited < wait_max) {
            # wait until the file is generated
            Sys.sleep(1)
            have_waited <- have_waited + 1
          }
          file.copy(tmp_file, file)
        }, contentType = "application/zip"
      )
    }
  )
}


sqliteDBZIPUI <- function(id) {

  ns <- NS(id)
  downloadButton(ns("button"), "DB .zip")
}

