get_sessions <- function() {
  cache_dir <- file.path(
    FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]], "sessions"
  )

  fslretho_sessions <- list.files(path = cache_dir)#,pattern = "rds")
  names(fslretho_sessions) <- fslretho_sessions %>% sapply(., function(x) substr(x, 1, 10))
  fslretho_sessions <- as.list(fslretho_sessions)
  fslretho_sessions <- ifelse(length(fslretho_sessions) == 0, list("Empty_cache" = ""), fslretho_sessions)
  return(fslretho_sessions)
}


saveLoadUI <- function(id) {

  ns <- shiny::NS(id)
  fslretho_sessions <- get_sessions()

  shiny::tagList(
    load_button <- tags$li(
      shiny::actionButton("load", "", icon = shiny::icon("upload")),
      class = "dropdown user user-menu"
    ),

    load_ui <- tags$li(
      shiny::selectizeInput(
        "rds_load", label = "", multiple = FALSE,
        selected = fslretho_sessions[[1]], choices = fslretho_sessions
      ),
      class = "dropdown user user-menu"
    ),

    save_button <- tags$li(
      shiny::actionButton("save", "", icon = shiny::icon("save")),
      class = "dropdown user user-menu"
    ),

    save_ui <- tags$li(
      shiny::textInput("rds_save", label = "", value = "", placeholder = "save.rds"),
      class = "dropdown user user-menu"
    )
  )

}
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

  save_load_ui <- saveLoadUI("saveLoadModule")


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
    .list	= save_load_ui,
    reload_button,
    tags$li(
      tags$div(id = 'dataset_title', class = 'mybox', style = "padding: 10px; border: black 2px solid",
               shiny::textOutput("dataset_name"),
      ), class = "dropdown user user-menu"
    )
  )
  return(header)
}
