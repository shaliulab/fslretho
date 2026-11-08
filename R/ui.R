APP_TITLE <- "FSLRetho v3"
# width of the title area (with the logos) and of the sidebar below it
SIDEBAR_WIDTH <- 290

#' Git commit the app was built from (written to /etc/fslretho_commit by the Dockerfile)
#' @noRd
app_commit <- function() {
  path <- "/etc/fslretho_commit"
  commit <- if (file.exists(path)) trimws(readLines(path, n = 1, warn = FALSE)) else ""
  if (length(commit) == 0 || commit %in% c("", "unknown")) NA_character_ else commit
}

#' The commit, linked to GitHub unless it was built with uncommitted changes (-dirty)
#' @noRd
commit_label <- function() {
  commit <- app_commit()
  if (is.na(commit)) return(NULL)
  label <- tags$code(commit, title = "Version of FSLRetho (git commit)")
  if (grepl("dirty", commit)) return(label)
  tags$a(label, href = paste0("https://github.com/shaliulab/fslretho/commit/", commit), target = "_blank", rel = "noopener")
}

#' Title of the top bar: the KU Leuven and VIB logos, then the name of the app
#' @noRd
header_title <- function() {
  tags$span(
    class = "fsl-title",
    tags$img(src = "fslretho/static/logos/logo1.png", alt = "KU Leuven", class = "fsl-logo"),
    tags$img(src = "fslretho/static/logos/logo2.png", alt = "VIB", class = "fsl-logo"),
    APP_TITLE
  )
}

#' A dropdown menu in the top bar
#' @noRd
header_dropdown <- function(id, title, ...) {
  shinydashboardPlus::dropdownBlock(id = id, title = title, div(class = "fsl-dropdown", ...))
}

get_header <- function() {
  shinydashboardPlus::dashboardHeader(
    title = header_title(), titleWidth = SIDEBAR_WIDTH,
    leftUi = tagList(
      header_dropdown("scoringInput", "Scoring parameters", scoringParamsUI("scoring")),
      header_dropdown("sleepBinningInput", "Sleep binning", binDataUI("sleepBin")),
      header_dropdown("boutBinningInput", "Bout binning", binDataUI("boutBin")),
      header_dropdown("downloadCenter", "Download datasets", downloadsUI("downloads")),
      header_dropdown("sessionsMenu", "Sessions", sessionsUI("sessions"))
    ),
    tags$li(class = "dropdown fsl-header-item", actionButton("reload", "", icon = icon("redo"), title = "Load the metadata again")),
    tags$li(class = "dropdown fsl-header-item", div(class = "fsl-dataset-name", textOutput("dataset_name", inline = TRUE))),
    tags$li(class = "dropdown fsl-header-item fsl-commit", commit_label())
  )
}

get_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    width = SIDEBAR_WIDTH,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      shinydashboard::menuItem("Load", tabName = "load", icon = icon("upload")),
      shinydashboard::menuItem("Snapshot viewer", tabName = "snapshot", icon = icon("file-video")),
      shinydashboard::menuItem("Raw", tabName = "raw", icon = icon("chart-line")),
      shinydashboard::menuItem("Premade", tabName = "premade", icon = icon("chart-bar")),
      shinydashboard::menuItem("Sleep analysis", tabName = "sleep", icon = icon("moon")),
      shinydashboard::menuItem("Bout analysis", tabName = "bout", icon = icon("moon")),
      shinydashboard::menuItem("Period analysis", tabName = "period", icon = icon("clock")),
      shinydashboard::menuItem("Revise metadata", tabName = "metadata", icon = icon("table")),
      shinydashboard::menuItem("Reproducibility script", tabName = "reproducibility", icon = icon("code")),
      shinydashboard::menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  )
}

get_body <- function() {
  tab <- shinydashboard::tabItem
  shinydashboard::dashboardBody(
    shinybusy::add_busy_bar(color = "#FF0000"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "fslretho/css/styles.css"),
      tags$script(src = "fslretho/js/my_javascript.js")
    ),
    shinydashboard::tabItems(
      tab("welcome", welcomePageUI("welcome")),
      tab("load", loadDataUI("loadData")),
      tab("snapshot", snapshotViewerUI("snapshotViewer")),
      tab("raw", rawPlotsUI("rawPlots")),
      tab("premade", premadePlotsUI("premadePlots")),
      tab("sleep", esquisseModuleUI("sleepPlot"), flySummaryUI("sleepSummary")),
      tab("bout", esquisseModuleUI("boutPlot"), flySummaryUI("boutSummary")),
      tab("period", periodAnalysisUI("periodAnalysis")),
      tab("metadata", viewMetadataUI("viewMetadata")),
      tab("reproducibility", reproducibilityModuleUI("reproducibility")),
      tab("help", helpPageUI("help"))
    )
  )
}

#' The UI of fslretho
#' @noRd
ui <- function() {
  shinydashboardPlus::dashboardPage(
    skin = "black", title = APP_TITLE,
    header = get_header(), sidebar = get_sidebar(), body = get_body()
  )
}
