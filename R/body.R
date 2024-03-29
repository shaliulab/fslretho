#' @importFrom shinybusy add_busy_bar
#' @importFrom shinydashboard dashboardBody tabItem tabItems box
#' @importFrom htmltools tagAppendAttributes
#' @importFrom shiny tags tagList plotOutput verbatimTextOutput tabsetPanel tabPanel
#' @importFrom esquisse esquisserUI
get_body <- function() {

  # TABS
# Welcome tab: the user lands here
  welcome_tab <- shinydashboard::tabItem(tabName = 'welcome', welcomePageUI("welcome"))

  load_tab <- shinydashboard::tabItem(tabName = 'load', loadDataUI("loadData"))
  raw_tab <- shinydashboard::tabItem(tabName = "raw", rawPlotsUI("rawPlots"))

  premade_tab <- shinydashboard::tabItem(tabName = "premade", premadePlotsUI("premadePlots"))

  sleep_tab <- shinydashboard::tabItem(tabName = 'sleep', esquisseModuleUI("sleepPlot"), esquisseModuleUI("sleepPlotSummary"))
  bout_tab <- shinydashboard::tabItem(tabName = 'bout', esquisseModuleUI("boutPlot"), esquisseModuleUI("boutPlotSummary"))
  period_tab <- shinydashboard::tabItem(tabName = 'period', periodAnalysisUI("periodAnalysis"))

  metadata_tab <- shinydashboard::tabItem(tabName = 'metadata', viewMetadataUI("viewMetadata"))
  # backup_tab <- shinydashboard::tabItem(tabName = 'backup', backupManagerUI("manageBackup"))
  reproducibility_tab <- shinydashboard::tabItem(tabName = 'reproducibility', reproducibilityModuleUI("reproducibility"))

  # snapshot_tab <- shinydashboard::tabItem(tabName = 'snapshot', snapshotViewerUI("snapshot_viewer"))

  # TODO Place somewhere the UI for scoreData
  shinydashboard::dashboardBody(
    shinybusy::add_busy_bar(color = "#FF0000"),
    tags$link(rel = "stylesheet", type = "text/css", href = "fslretho/css/styles.css"),
    tags$script(src = "fslretho/js/my_javascript.js"),

    shinydashboard::tabItems(
      welcome_tab,
      load_tab,
      # snapshot_tab,
      raw_tab,
      premade_tab,
      sleep_tab,
      bout_tab,
      period_tab,
      metadata_tab,
      # backup_tab,
      reproducibility_tab
    )
  )

}
