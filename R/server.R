#' Server function of FSLRetho
#'
#' Datasets flow between modules as reactiveValues with slots data, name and time (see dataset_rv).
#' @noRd
server <- function(input, output, session) {

  track_usage()
  welcomePageServer("welcome", session)

  ## Load ----
  scoring <- scoringParamsServer("scoring")
  load <- loadDataServer("loadData", reload = reactive(input$reload), params = scoring, parent_session = session)
  # the last dataset loaded, either from the database or from a saved session
  loaded_data <- sessionsServer("sessions", load$dataset)

  output$dataset_name <- renderText(if (is.null(loaded_data$name)) "No data loaded" else loaded_data$name)
  viewMetadataServer("viewMetadata", loaded_data)
  # the Raw tab opens the snapshot closest to the time clicked
  snapshot_request <- reactiveVal(NULL)
  snapshotViewerServer("snapshotViewer", loaded_data, snapshot_request)

  ## Bin ----
  sleep_data <- binDataServer("sleepBin", loaded_data)
  interactions_data <- binDataServer("interactionsBin", loaded_data, summary_time_window = 30, summary_FUN = "mean")
  bout_data <- binDataServer("boutBin", loaded_data, preproc_FUN = bout_analysis)

  ## Plot ----
  premadePlotsServer("premadePlots", sleep_data, interactions_data)
  raw_data <- rawPlotsServer("rawPlots", sleep_data, interactions_data, snapshot_request, session)

  sleep_plot <- esquisseModuleServer(
    "sleepPlot", sleep_data, x_unit = c(t = "hours"),
    hardcoded_dragula = list(mapping = list(xvar = "t", yvar = "asleep"), geom = "pop_etho")
  )
  bout_plot <- esquisseModuleServer(
    "boutPlot", bout_data, x_unit = c(t = "hours"),
    hardcoded_dragula = list(mapping = list(xvar = "t", yvar = "duration"), geom = "pop_etho")
  )

  # one value per fly, from the data shown in the plots above
  # Y axis of the first plot of each tab
  sleep_y <- reactive(input[["sleepPlot-esquisse-dragvars"]]$target$yvar[1])
  bout_y <- reactive(input[["boutPlot-esquisse-dragvars"]]$target$yvar[1])
  sleep_summary <- flySummaryServer("sleepSummary", sleep_plot, loaded_data,
                                    vars = c("asleep", "p_wake", "p_doze", "activity", "moving", "interactions", "max_velocity"),
                                    plot_y = sleep_y)
  bout_summary <- flySummaryServer("boutSummary", bout_plot, loaded_data, vars = "duration", plot_y = bout_y)

  ## Period ----
  # the period analysis uses the sleep data with the filters of the sleep plot
  sleep_filtered <- esquisseReplayServer("replay", sleep_data, sleep_plot)
  rhythm <- periodAnalysisServer("periodAnalysis", sleep_filtered)

  ## Export ----
  downloadsServer(
    "downloads",
    datasets = list(
      `sleep-binned` = sleep_plot,
      `sleep-scored` = loaded_data,
      `raw-data` = raw_data,
      `bouts` = bout_plot,
      `sleep-summary` = sleep_summary,
      `bouts-summary` = bout_summary,
      `periodogram` = rhythm$periodogram,
      `spectrogram` = rhythm$spectrogram
    ),
    loaded = loaded_data,
    plot_y = list(`sleep-binned` = sleep_y, `bouts` = bout_y),
    summary_fun = list(
      `sleep-binned` = reactive(input[["sleepBin-summary_FUN"]]),
      `bouts` = reactive(input[["boutBin-summary_FUN"]])
    )
  )

  reproducibilityModuleServer(
    "reproducibility", loaded_data,
    params = reactive(list(
      monitor = if (is_dam_dataset(req(loaded_data$data))) "dam" else "ethoscope",
      result_dir = if (is_dam_dataset(loaded_data$data)) load$result_dir_dam() else load$result_dir(),
      scoring = scoring(),
      # the flies selected in the Load tab (NULL: all of them, or a saved session)
      ids = {
        selected <- load$selected_ids()
        if (!is.null(selected) && setequal(selected, as.character(behavr::meta(loaded_data$data)$id))) selected
      },
      summary_time_window = input[["sleepBin-summary_time_window"]],
      summary_FUN = input[["sleepBin-summary_FUN"]]
    ))
  )
}
