#' @importFrom shinydashboardPlus dashboardHeader dropdownBlock
#' @importFrom shiny actionButton tags selectizeInput textOutput tagList
#' @importFrom shinydashboardPlus dashboardHeader
get_header <- function() {

  ## Components

  if (FSLRethoConfiguration$new()$content$debug) {
    browserButton <- actionButton("browser", label = "Browser", class = "dropdown user user-menu")
  } else {
    browserButton <- tags$div(style = "hidden", class = "dropdown user user-menu")
  }

  scoring_ui <- shinydashboardPlus::dropdownBlock(
    id = "scoringInput",
    title = "Scoring parameters",
    scoreDataUI("loadData-annotation")
  )

  sleep_binning_ui <- shinydashboardPlus::dropdownBlock(
    id = "sleep_binningInput",
    title = "Sleep binning parameters",
    binDataUI("sleepBin", binning_variable = "asleep")
  )

  bout_binning_ui <- shinydashboardPlus::dropdownBlock(
    id = "bout_binningInput",
    title = "Bout binning parameters",
    binDataUI("boutBin", binning_variable = "duration")
  )

  sleep_summary_ui <- shinydashboardPlus::dropdownBlock(
    id = "sleepSummaryInput",
    title = "Sleep summary",
    summaryStatisticUI("sleepSummary", var="asleep")
  )

  bout_summary_ui <- shinydashboardPlus::dropdownBlock(
    id = "boutSummaryInput",
    title = "Bout summary",
    summaryStatisticUI("boutSummary", var="duration")
  )

  download_ui <- shinydashboardPlus::dropdownBlock(
    id = "download_center",
    title = "Download datasets",
    tagList(
      sqliteDBZIPUI("sqliteDB"),
      downloadServerUI("binned-sleep", "Sleep binned"),
      downloadServerUI("sequence-sleep", "Sleep sequence"),
      downloadServerUI("raw-data", "Sleep raw"),
      downloadServerUI("bouts-sleep", "Sleep bouts"),
      downloadServerUI("sleep-summary", "Sleep summary"),
      downloadServerUI("bouts-summary", "Sleep bouts summary"),
      downloadServerUI("periodogram_dataset", "Periodogram dataset"),
      downloadServerUI("spectrogram_dataset", "Spectrogram dataset")
    )
  )

  saveload_ui <- saveLoadSessionUI("sessions")
  # save_ui <- saveSessionUI("sessions-save")
  # load_ui <- loadSessionUI("sessions-load")
  reload_button <- reloadUI("reload")

  debug_ui <- debugModuleUI("debug")

  ### Put togethero
  header <- shinydashboardPlus::dashboardHeader(
    title = "FSLRetho2",
    # stuff to the left of the navbar
    leftUi = tagList(
      browserButton,
      scoring_ui,
      sleep_binning_ui,
      bout_binning_ui,
      sleep_summary_ui,
      bout_summary_ui,
      download_ui,
      debug_ui,

      # save_ui[[1]], save_ui[[2]]
      # save_ui

    ),
    # .list = append(save_ui, load_ui),
    .list = saveload_ui,

    # stuff to the right
    # load_ui,
    # load_ui,
    reload_button,
    tags$li(
      tags$div(id = 'dataset_title', class = 'mybox', style = "padding: 10px; border: black 2px solid",
               textOutput("dataset_name"),
      ), class = "dropdown user user-menu"
    )
  )
  return(header)
}
