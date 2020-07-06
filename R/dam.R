loadDamServer <- function(id, last_monitor, dataset_name){

  moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({

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

        fortify(fsldamr::load_dam(metadata_linked()), meta = TRUE)
      })

      # make it eager
      observeEvent(input$submit, {
        dt_raw()
        last_monitor("dam")
        dataset_name(input$metadata$name)
      })
      return(dt_raw)
    }
  )
}