saveLoadServer <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {
      cache_dir <- file.path(
        FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]], "sessions"
      )

      observeEvent(input$save, {
        tosave <- reactiveValuesToList(data)

        saveRDS(
          object = tosave,
          file = file.path(cache_dir, input$rds_save)
        )
      })

      saved_unified_data <- reactive({
        readRDS(
          file = file.path(cache_dir, input$rds_load)
        )
      })

      return(saved_unified_data)
    }
  )

}

#' Server function of FSLRetho
#'
#' @import shiny
#' @import behavr
#' @importFrom shinylogs track_usage store_json
#' @importFrom esquisse esquisserServer
#' @importFrom sleepr bout_analysis
#' @importFrom ggplot2 ggplot facet_wrap aes facet_grid
#' @importFrom ggetho stat_ld_annotations stat_pop_etho
#' @importFrom cowplot plot_grid
#' @importFrom rlang expr
#' @noRd
server <- function(input, output, session) {

  reload_button <- function() {
    reactive({
      req(!is.null(input$reloadData))
      input$reloadData
    })
  }

  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))


  ## Preparation ----
  # Run a simple ethoscope backup manager
  backupManagerServer("manageBackup")

  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", unified_data)


  # Define a trigger shared across modules
  reload <- reload_button()

  ## Load ----
  raw_data <- loadDataServer("loadData", reload)

  ## Score ----
  scored_data <- scoreDataServer("scoreData", raw_data)


  ## Bin  sleep ----
  sleep_data <- binDataServer("sleepData", scored_data)
  sleep_data_rejoined <- reactive({
    rejoin(binned_data())
  })


  ## Bin bouts ----
  bout_data   <- binDataServer("boutData", unified_data, preproc_FUN = bout_analysis, var = "asleep")
  bout_data_rejoined <- reactive({
    rejoin(bout_data())
  })

  ## Plot ----
  # Plot sleep result
  plotServer("sleepPlot", sleep_data)

  # Plot bout result
  plotServer("boutPlot", bout_data)


#
#   # Save session
#   saved_unified_data <- saveLoadServer(id="saveLoadModule", data=unified_data)
#
#   # Load a past session
#   observeEvent(input$`load-saveModule`, {
#     unified_data$data <<- saved_unified_data()$data
#     unified_data$name <<- saved_unified_data()$name
#     unified_data$time <<- saved_unified_data()$time
#   })




}