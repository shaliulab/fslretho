#'
#' @importFrom shinydashboardPlus dashboardHeader dropdownBlock
#' @importFrom shiny actionButton tags selectizeInput textOutput tagList
#' @importFrom shinydashboardPlus dashboardHeader
get_header <- function() {

  ## Components

  if (FSLRethoConfiguration$new()$content$debug) {
    browserButton <- shiny::actionButton("browser", label = "Browser", class = "dropdown user user-menu")
  } else {
    browserButton <- shiny::tags$div(style = "hidden", class = "dropdown user user-menu")
  }

  scoring_ui <- shinydashboardPlus::dropdownBlock(
    id = "scoringInput",
    title = "Scoring parameters",
    scoreDataUI("scoreData")
  )

  binning_ui <- shinydashboardPlus::dropdownBlock(
    id = "binningInput",
    title = "Binning parameters",
    binDataUI("binData")
  )

  load_button <- tags$li(
    shiny::actionButton("load", "", icon = shiny::icon("upload")),
    class = "dropdown user user-menu"
  )

  fslretho_sessions <- get_sessions()

  load_ui <- tags$li(
    shiny::selectizeInput(
      "rds_load", label = "", multiple = FALSE,
      selected = fslretho_sessions[[1]], choices = fslretho_sessions
    ),
    class = "dropdown user user-menu"
  )

  save_button <- tags$li(
    shiny::actionButton("save", "", icon = shiny::icon("save")),
    class = "dropdown user user-menu"
  )

  save_ui <- tags$li(
    shiny::textInput("rds_save", label = "", value = "", placeholder = "save.rds"),
    class = "dropdown user user-menu"
  )

  reload_button <- tags$li(
    shiny::actionButton("reloadData", "", icon = shiny::icon("redo")),
    class = "dropdown user user-menu"
  )

  ### Put together
  header <- shinydashboardPlus::dashboardHeader(
    title = "FSLRetho2",
    # stuff to the left of the navbar
    leftUi = shiny::tagList(
      browserButton,
      scoring_ui,
      binning_ui,
    ),
    # stuff to the right
    load_button,
    load_ui,
    save_button,
    save_ui,
    reload_button,
    tags$li(
      tags$div(id = 'dataset_title', class = 'mybox', style = "padding: 10px; border: black 2px solid",
               shiny::textOutput("dataset_name"),
      ), class = "dropdown user user-menu"
    )
  )
  return(header)
}