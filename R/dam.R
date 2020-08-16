loadDamServer <- function(id, reload){

  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      metadata <- reactive({

        reload()

        withCallingHandlers(
          expr = tryCatch({
            load_metadata(input$metadata$datapath, "dam")
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
        fsldamr::link_dam_metadata(metadata(), input$result_dir)
      })


      dt_raw <- reactive({

        if (nrow(metadata_linked()) == 0) {
          showNotification("Failure: no matches were found in the local DAM database.
                           This could be due to typos in the columns of the metadata or
                           your dataset being missing in the database.
                           Check your metadata and/or the local database to find out which is the problem", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        } else {
          showNotification("Success")
        }
        message("Loading dam data")

        # TODO Can this all be packaged into a function?
        progress <- shiny::Progress$new()
        on.exit(progress$close())

        progress$set(message = "Loading ROI number ", value = 0)
        n <- length(unique(metadata()$file))

        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1 / n, detail = detail)
        }

        dt_raw <- fsldamr::load_dam(metadata_linked(), FUN = NULL, updateProgress = updateProgress) %>%
          fortify(., meta = TRUE)

        attr(dt_raw, "monitor") <- "dam"
        dt_raw
      })

      dt_raw_validated <- reactive({
        if (nrow(dt_raw()) == 0) {
          showNotification("Failure: your metadata could be linked but the resulting table is empty", type = "error")
          shiny::validate(shiny::need(FALSE, label = ""))
        }

        dt_raw()
      })

      observeEvent(c(input$submit, reload()), {
        # TODO Make sure here that dt_raw_validated() attr monitor is still dam
        rv$data <- dt_raw_validated()
        rv$name <- input$metadata$name
        rv$time <- as.numeric(Sys.time())
      }, ignoreInit = TRUE)


      return(rv)
    }
  )
}