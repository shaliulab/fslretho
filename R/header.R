get_sessions <- function() {
  cache_dir <- file.path(
    FSLRethoConfiguration$new()$content$scopr$folders$cache$path, "sessions"
  )

  sessions <- list.files(path = cache_dir)#,pattern = "rds")
  names(sessions) <- sessions %>% sapply(., function(x) substr(x, 1, 10))
  sessions <- as.list(sessions)
  sessions <- ifelse(length(sessions) == 0, list("Empty_cache" = ""), sessions)
  return(sessions)
}


loadSessionUI <- function(id) {

  ns <- NS(id)
  sessions <- get_sessions()

  tagList(
    load_button <- tags$li(
      actionButton("load", "", icon = icon("upload")),
      class = "dropdown user user-menu"
    ),

    load_ui <- tags$li(
      selectizeInput(
        "rds_load", label = "", multiple = FALSE,
        selected = sessions[[1]], choices = sessions
      ),
      class = "dropdown user user-menu"
    )
  )
}

saveSessionUI <- function(id) {

  ns <- NS(id)

  tagList(
    save_button <- tags$li(
      actionButton("save", "", icon = icon("save")),
      class = "dropdown user user-menu"
    ),

    save_ui <- tags$li(
      textInput("rds_save", label = "", value = "", placeholder = "save.rds"),
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
    browserButton <- actionButton("browser", label = "Browser", class = "dropdown user user-menu")
  } else {
    browserButton <- tags$div(style = "hidden", class = "dropdown user user-menu")
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

  save_ui <- saveSessionUI("saveSession")
  load_ui <- loadSessionUI("loadSession")
  reload_button <- reloadUI("reload")

  ### Put together
  header <- shinydashboardPlus::dashboardHeader(
    title = "FSLRetho2",
    # stuff to the left of the navbar
    leftUi = tagList(
      browserButton,
      scoring_ui,
      binning_ui,
    ),
    # stuff to the right
    .list	= load_ui, save_ui,
    reload_button,
    tags$li(
      tags$div(id = 'dataset_title', class = 'mybox', style = "padding: 10px; border: black 2px solid",
               textOutput("dataset_name"),
      ), class = "dropdown user user-menu"
    )
  )
  return(header)
}
