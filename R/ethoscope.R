#' Wrap the ethoscope loading functionality for a ShinyUI
#' @importFrom magrittr `%>%`
#' @importFrom fslscopr link_ethoscope_metadata load_ethoscope
#' @import shiny
#' @noRd
loadEthoscopeServer <- function(id, metadata_datapath, reload, input, session) {

      rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      ethoscope_metadata_datapath <- reactive({
        req(metadata_datapath())
        metadata_datapath()[sapply(metadata_datapath(), get_monitor_name) == "ethoscope"]
      })


      metadata <- reactive({

        reload()
        # browser()
        req(ethoscope_metadata_datapath())

        withCallingHandlers(
            expr = tryCatch({
              load_metadata(ethoscope_metadata_datapath(), monitor = "ethoscope")
            }, error = function(e) {
              show_condition_message(e, "error", session)
              list(plot = NULL, data = NULL, layout = NULL)
              shiny::validate(shiny::need(expr = F, label = "metadata is not valid"))
            }
            ),
            warning = function(w) {
              show_condition_message(w, "warning", session)
              list(plot = NULL, data = NULL, layout = NULL)
            }
          )

      })

      metadata_linked <- reactive({
        fslscopr::link_ethoscope_metadata(x = metadata(), result_dir = input$result_dir_ethoscope)
      })


      dt_raw <- reactive({

        if (nrow(metadata_linked()) == 0) {
          shiny::showNotification("Failure: no matches were found in the local ethoscope database.
                           This could be due to typos in the machine_name, date, etc; or
                           your dataset being missing in the database.
                           Check your metadata and/or the local database to find out which is the problem", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        } else {
          showNotification("Success")
        }
        message("Loading ethoscope data")

        # TODO Can this all be packaged into a function?
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "", value = 0)
        n <- nrow(metadata())

        updateProgress <- function(detail = NULL) {
          if (FSLRethoConfiguration$new()$content[["ncores"]] == 1) {
            progress$inc(amount = 1 / n, detail = detail)
          } else {
            shiny::showNotification(detail, type = "message", duration = 5)
          }
        }


        dt_raw <- fslscopr::load_ethoscope(
          metadata = metadata_linked(),
          reference_hour = NA,
          ncores = FSLRethoConfiguration$new()$content[["ncores"]],
          cache = FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]],
          verbose = TRUE,
          updateProgress = updateProgress
        )  %>%
          fortify(., meta = TRUE)

        attr(dt_raw, "monitor") <- "ethoscope"
        dt_raw
      })

      dataset_name <- reactive({
        res <- basename(metadata_datapath())
        print(res)
        res
      })


      dt_raw_validated <- reactive({
        if (nrow(dt_raw()) == 0) {
          showNotification("Failure: your metadata could be linked but the resulting table is empty", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        }

        dt_raw()
      })

      observeEvent(c(input$submit, reload()), {
        # browser()
        if(isTruthy(ethoscope_metadata_datapath())) {
          rv$data <- dt_raw_validated()
          rv$name <- dataset_name()
          rv$time <- as.numeric(Sys.time())
        } else {
          rv$data <- NULL
          rv$name <- NULL
          rv$time <- NULL
        }

    }, ignoreInit = TRUE)

    return(rv)
}