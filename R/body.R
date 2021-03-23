tabPanelUI <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    shiny::tabPanel(
      title = title,
      esquisse::esquisserUI(
        id = ns(id),
        header = FALSE, # dont display gadget title
        choose_data = FALSE, # dont display button to change data
        disable_filters = FALSE
      )
    ),
    shiny::tabPanel(
      title = paste0(title, "_out"),
      shiny::verbatimTextOutput(ns(paste0(id, "_out")))
    )
  )
}

get_plot_tab <- function(ids, titles, name) {

  panel_list <- shiny::tagList()

  i <- 1
  for (id in ids) {
    panel_list[[i]] <- tabPanelUI(ids[i], titles[i])
    i <- i + 1
  }

  shinydashboard::tabItem(tabName = name,
    panel_list
  )
}


get_sleep_tab <- function() {
  get_plot_tab(
    ids = c("analyseSleep_01", "analyseSleep_02"),
    title = c("Plot", "Output"),
    name = "sleep"
  )
}

get_bout_tab <- function() {
  get_plot_tab(
    ids = c("analyseBout_01", "analyseBout_02"),
    title = c("Plot", "Output"),
    name = "bout"
  )
}


# get_bout_ui <- function() {
#   shinydashboard::tabItem(tabName = "bout",
#     shiny::tabsetPanel(
#       shiny::tabPanel(
#         title = "Bout analysis",
#         esquisse::esquisserUI(
#           id = "analyseBout_01",
#           header = FALSE, # dont display gadget title
#           choose_data = FALSE, # dont display button to change data
#           disable_filters = FALSE
#         )
#       ),
#       shiny::tabPanel(
#         title = "bout output",
#         shiny::verbatimTextOutput("analyseBout_01_out")
#       )
#     ),
#     shiny::tabsetPanel(
#       shiny::tabPanel(
#         title = "Bout analysis",
#         esquisse::esquisserUI(
#           id = "analyseBout_02",
#           header = FALSE, # dont display gadget title
#           choose_data = FALSE, # dont display button to change data
#           disable_filters = FALSE
#         )
#       ),
#       shiny::tabPanel(
#         title = "bout output",
#         shiny::verbatimTextOutput("analyseBout_02_out")
#       )
#     )
#   )
# }


# get_sleep_tab <- function() {
#   shinydashboard::tabItem(tabName = "sleep",
#     # shiny::fluidRow(
#     #   shinydashboard::box(
#     #     title = "Sleep trace + interactions", status = "primary", solidHeader = TRUE,
#     #     collapsible = TRUE, width = "90%",
#     #     shiny::plotOutput("analyseSleep_00", height = "400px") %>%
#     #       htmltools::tagAppendAttributes(class = "resizable") %>%
#     #       htmltools::tagAppendAttributes(onmouseup = 'refresh_plot()')
#     #   )
#     # ),
#
#     analyse_sleep_01_help(),
#
#     shiny::tabsetPanel(
#       shiny::tabPanel(
#         title = "Plot",
#         esquisse::esquisserUI(
#           id = "analyseSleep_01",
#           header = FALSE, # dont display gadget title
#           choose_data = FALSE, # dont display button to change data
#           disable_filters = FALSE
#         )
#       ),
#       shiny::tabPanel(
#         title = "Output",
#         shiny::verbatimTextOutput("analyseSleep_01_out")
#       )
#     ),
#
#     analyse_sleep_02_help(),
#
#
#     shiny::tabsetPanel(
#       shiny::tabPanel(
#         title = "Plot",
#         esquisse::esquisserUI(
#           id = "analyseSleep_02",
#           header = FALSE, # dont display gadget title
#           choose_data = FALSE, # dont display button to change data
#           disable_filters = FALSE
#         )
#       ),
#       tabPanel(
#         title = "Output",
#         shiny::verbatimTextOutput("analyseSleep_02_out")
#       )
#     )
#   )
# }

#' @importFrom shinybusy add_busy_bar
#' @importFrom shinydashboard dashboardBody tabItem tabItems box
#' @importFrom htmltools tagAppendAttributes
#' @importFrom shiny tags tagList plotOutput verbatimTextOutput tabsetPanel tabPanel
#' @importFrom esquisse esquisserUI
get_body <- function() {

  # TABS
# Welcome tab: the user lands here
  welcome_tab <- shinydashboard::tabItem(tabName = 'welcome', welcomePageUI())
  load_tab <- shinydashboard::tabItem(tabName = 'load', loadDataUI("loadData"))

  sleep_tab <- get_sleep_tab()
  bout_tab <- get_bout_tab()

  metadata_tab <- shinydashboard::tabItem(tabName = 'metadata', viewMetadataUI("viewMetadata"))
  backup_tab <- shinydashboard::tabItem(tabName = 'backup', backupManagerUI("manageBackup"))


  # TODO Place somewhere the UI for scoreData
  shinydashboard::dashboardBody(
    shinybusy::add_busy_bar(color = "#FF0000"),
    tags$link(rel = "stylesheet", type = "text/css", href = "fslretho/css/styles.css"),
    tags$script(src = "fslretho/js/my_javascript.js"),

    shinydashboard::tabItems(
      welcome_tab,
      load_tab,
      sleep_tab,
      bout_tab,
      metadata_tab,
      backup_tab

    )
  )

}
