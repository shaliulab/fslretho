#' Server function of FSLRetho
#'
#' @import shiny
#' @import behavr
#' @importFrom shinylogs track_usage store_json
#' @importFrom sleepr bout_analysis
#' @importFrom ggplot2 ggplot facet_wrap aes facet_grid
#' @importFrom ggetho stat_ld_annotations stat_pop_etho
#' @importFrom cowplot plot_grid
#' @importFrom rlang expr
#' @noRd
server <- function(input, output, session) {

  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  # Define a trigger shared across modules
  reload <- reloadModuleServer("reload")

  ## Preparation ----
  # Run a simple ethoscope backup manager
  backupManagerServer("manageBackup")

  ## Load ----
  # Here the choice between dam or ethoscope happens
  # After this, the data has only one module
  raw_data <- loadDataServer("loadData", reload)

  # In case the user wants to use a builtin dataset
  loaded_data <- saveLoadSessionServer("sessions", raw_data)

  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", loaded_data)

  ## Score ----
  scored_data <- scoreDataServer("scoreData", loaded_data)
  selected_data <- monitorSelectorServer("selectData", scored_data)

  ## Bin sleep ----
  sleep_data <- binDataServer("sleepData", selected_data)

  ## Bin bouts ----
  bout_data <- binDataServer("boutData", loaded_data, preproc_FUN = bout_analysis, var = "asleep")

  observeEvent(sleep_data$data, {

    browser()
  }, ignoreInit=TRUE)

  ## Plot ----
  # Plot sleep result
  plotServer("sleepPlot", sleep_data)

  # Plot bout result
  plotServer("boutPlot", bout_data)
}
