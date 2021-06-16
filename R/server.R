builtInOrNewDataServer <- function(id, input_rv) {

  output_rv <- reactiveValues(data=NULL, name=NULL, time=NULL)

  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input_rv$time, {
        output_rv$data <- input_rv$data
        output_rv$name <- input_rv$name
        output_rv$time <- input_rv$time
      })

      observeEvent(input$load, {
        read_rv <- read_reativeValuesRDS(input$rds_load)
        output_rv$data <- read_rv$data
        output_rv$name <- read_rv$name
        output_rv$time <- read_rv$time
      })

      return(output_rv)
    }
  )
}


saveDataServer <- function(id, input_rv) {

  cache_path <- FSLRethoConfiguration$new()$content$scopr$folders$cache$path

  moduleServer(
    id,
    function(input, output, session) {
      cache_dir <- file.path(cache_path, "sessions")

      observeEvent(input$save, {
        save_reativeValuesRDS(
          object = input_rv$data,
          file = file.path(cache_dir, input$rds_save)
        )
      })
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


  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  reload <- reloadModuleServer("reload")

  ## Preparation ----
  # Run a simple ethoscope backup manager
  backupManagerServer("manageBackup")

  # Define a trigger shared across modules
  reload <- reload_button()

  ## Load ----
  # Here the choice between dam or ethoscope happens
  # After this, the data has only one module
  raw_data <- loadDataServer("loadData", reload)

  # In case the user wants to use a builtin dataset
  loaded_data <- builtInOrNewDataServer("loadSession", raw_data)
  saveDataServer("saveSession", loaded_data)

  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", loaded_data)


  ## Score ----
  scored_data <- scoreDataServer("scoreData", loaded_data)

  ## Bin sleep ----
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
}
