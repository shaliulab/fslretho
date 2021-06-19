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
    scoreDataUI("scoreData")
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

  download_ui <- shinydashboardPlus::dropdownBlock(
    id = "download_center",
    title = "Download datasets",
    tagList(
      downloadServerUI("sleep_download", "Sleep"),
      downloadServerUI("sleep_bout_download", "Sleep bouts")
    )
  )

  saveload_ui <- saveLoadSessionUI("sessions")
  # save_ui <- saveSessionUI("sessions-save")
  # load_ui <- loadSessionUI("sessions-load")
  reload_button <- reloadUI("reload")

  ### Put togethero
  header <- shinydashboardPlus::dashboardHeader(
    title = "FSLRetho2",
    # stuff to the left of the navbar
    leftUi = tagList(
      browserButton,
      scoring_ui,
      sleep_binning_ui,
      bout_binning_ui,
      download_ui,

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
