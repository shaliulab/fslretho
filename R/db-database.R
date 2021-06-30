
#' @importFrom behavr meta
sqliteDBZIPServer <- function(id, input_rv, monitor) {

  moduleServer(
    id,
    function(input, output, session) {


      temp_file <- reactiveVal(NULL)
      ids <- character(0)
      n <- 0

      observeEvent(input$generate, {
        active_notification <- showNotification("Generating .zip file", duration = NULL)
        ids <<- c(ids, active_notification)
        n <<- n + 1

        req(monitor() == "ethoscope")
        req(input_rv$data)
        dbfiles <- unique(behavr::meta(input_rv$data)$file_info)
        message(dbfiles)
        tmp_file <- tempfile(fileext = ".zip")
        temp_file(tmp_file)
        message(paste0("This is the temporary .zip: ", tmp_file))
        zip_database(tmp_file, dbfiles)
      })

      output$download <- downloadHandler(

        filename = function() {
          req(input_rv$name)
          tmp_file <- tempfile(pattern = paste0(input_rv$name, "sqliteDB"), fileext = ".zip")
          message(tmp_file)
          tmp_file
        },
        content = function(file) {
          if (!file.exists(file)) {
            showNotification("Zip file not ready yet", duration = 3)
            return(NULL)
          } else {
            file.copy(temp_file(), file)
            if (length(ids) > 0) {
              removeNotification(ids[n])
              ids <<- ids[-n]
              n <<- n - 1
            }
          }
        }, contentType = "application/zip"
      )
    }
  )
}


sqliteDBZIPUI <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("generate"), "Generate"),
    downloadButton(ns("download"), "DB .zip")
  )
}

