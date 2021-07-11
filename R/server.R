#' Server function of FSLRetho
#'
#' @import shiny
#' @importFrom rlang current_env
#' @noRd
server <- function(input, output, session) {

  welcomePageServer("welcome")
  track_usage()

  # Define a trigger shared across modules
  reload <- reloadModuleServer("reload")
  debugModuleServer("debug", envir = rlang::current_env())

  ## Preparation ----
  # Run a simple ethoscope backup manager
  backupManagerServer("manageBackup")

  ## Load ----
  # Here the choice between dam or ethoscope happens
  # After this, the data has only one module
  scored_data <- loadDataServer("loadData", reload)

  # In case the user wants to use a builtin dataset
  loaded_data <- saveLoadSessionServer("sessions", scored_data)

  snapshotViewerServer("snapshot_viewer", loaded_data)

  ## Score ----
  # scored_data <- scoreDataServer("scoreData", loaded_data)
  selected_data <- monitorSelectorServer("selectData", loaded_data)
  monitor <- reactive({
    selected_data$monitor
  })

  sqliteDBZIPServer("sqliteDB", selected_data, monitor)

  reproducibilityModuleServer("reproducibility", envir = rlang::current_env())

  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", selected_data)

  ## Bin sleep ----
  sleep_data <- binDataServer("sleepBin", selected_data, allow_pareto=TRUE)
  interactions_data <- binDataServer(
    "interactionsBin", selected_data,
    y="interactions", summary_time_window = 30, summary_FUN = "mean"
  )

  premadePlotsServer("premadePlots", sleep_data, interactions_data)
  raw_datasets <- rawPlotsServer("rawPlots", sleep_data)

  ## Bin bouts ----
  bout_data <- binDataServer("boutBin", selected_data,
                             # compute bouts
                             preproc_FUN = bout_analysis,
                             # of the var asleep
                             var = "asleep")

  ## Plot ----
  # Plot sleep result

  sleep_module <- esquisseModuleServer(
    "sleepPlot", sleep_data,
    # pass this from the conf
    x_unit = c(t = "hours"),
    hardcoded_dragula = list(
      mapping = list(xvar="t", yvar="asleep"),
      geom = "pop_etho"
    )
  )
  sleep_bout_module <- esquisseModuleServer(
    "boutPlot", bout_data,
    hardcoded_dragula = list(
      mapping = list(xvar="t", yvar="duration"),
      geom = "pop_etho"
    ),
    # pass this from the conf
    x_unit = c(t = "hours")
  )


  # TODO summaryStatisticServer module can probably be replaced with binDataServer using
  # x_bin_length=Inf (make a single bin for all the data)
  sleep_summary <- summaryStatisticServer("sleepSummary", sleep_module)
  bout_summary <- summaryStatisticServer("boutSummary", sleep_bout_module)

  sleep_module_summary <- esquisseModuleServer(
    "sleepPlotSummary", sleep_summary,
    hardcoded_dragula = list(
      mapping = list(xvar="id", yvar="mean-asleep"),
      geom = "bar"
    )
  )
  sleep_bout_module_summary <- esquisseModuleServer(
    "boutPlotSummary", bout_summary,
      hardcoded_dragula = list(
        mapping = list(xvar="id", yvar="mean-duration"),
        geom = "bar"
      )
  )

  # apply the filters from esquisse to the original data
  sleep_replay <- esquisseReplayServer("replay", sleep_data, sleep_module)
  circadian_data <- periodAnalysisServer("periodAnalysis", sleep_replay)


  downloadServer("binned-sleep", sleep_module, sleep_data$name)
  downloadServer("sequence-sleep", loaded_data, selected_data$name, monitor)
  downloadServer("bouts-sleep", sleep_bout_module, selected_data$name)
  downloadServer("sleep-summary", sleep_summary, sleep_module_summary$name)
  downloadServer("bouts-summary", sleep_bout_module_summary, sleep_bout_module_summary$name)
  downloadServer("periodogram_dataset", circadian_data$periodogram, circadian_data$periodogram$name)
  downloadServer("spectrogram_dataset", circadian_data$spectrogram, circadian_data$spectrogram$name)
}
