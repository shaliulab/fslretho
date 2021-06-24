########################################################
# Not needed, only for readability/maintenance purposes
########################################################
#include "R/body.R"
#include "R/header.R"
########################################################

#' @importFrom shinydashboard menuItem sidebarMenu dashboardSidebar
#' @importFrom shiny icon
get_sidebar <- function() {
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Welcome", tabName = 'welcome', icon = icon('info')),
      shinydashboard::menuItem("Load", tabName = 'load', icon = icon('upload')),
      shinydashboard::menuItem("Snapshot viewer", tabName = 'snapshot', icon = icon('file-video')),
      shinydashboard::menuItem("Raw", tabName = 'raw'),
      shinydashboard::menuItem("Premade", tabName = 'premade'),
      shinydashboard::menuItem("Sleep analysis", tabName = 'sleep', icon = icon('moon')),
      shinydashboard::menuItem("Bout analysis", tabName = 'bout', icon = icon('moon')),
      shinydashboard::menuItem("Revise metadata", tabName = 'metadata', icon = icon('table')),
      shinydashboard::menuItem("Etho backup manager", tabName = 'backup', icon = icon('save')),
      shinydashboard::menuItem("Reproducibility script", tabName = 'reproducibility', icon = icon('info'))
    )
  )
}
#' A UI for fslretho
#'
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader
#' @importFrom shinydashboard menuItem tabItems tabItem dashboardBody
#' @importFrom shiny icon actionButton
#' @importFrom esquisse esquisserUI
#' @importFrom magrittr `%>%`
#' @importFrom shinybusy add_busy_bar
shinydashboard_ui <- function() {

  header <- get_header()
  sidebar <- get_sidebar()
  body <- get_body()

  ui <- shinydashboardPlus::dashboardPage(skin = "black"
                                          , header
                                          , sidebar
                                          , body
                                          )
  return(ui)

}

ui <- shinydashboard_ui
