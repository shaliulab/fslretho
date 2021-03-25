########################################################
# Not needed, only for readability/maintenance purposes
########################################################
#include "R/body.R"
#include "R/header.R"
########################################################

#' @impotFrom shinydashboard menuItem sidebarMenu dashboardSidebar
#' @importFrom shiny icon
get_sidebar <- function() {
  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Welcome", tabName = 'welcome', icon = shiny::icon('info')),
      shinydashboard::menuItem("Load", tabName = 'load', icon = shiny::icon('upload')),
      shinydashboard::menuItem("Sleep analysis", tabName = 'sleep', icon = shiny::icon('moon')),
      shinydashboard::menuItem("Bout analysis", tabName = 'bout', icon = shiny::icon('moon')),
      shinydashboard::menuItem("Revise metadata", tabName = 'metadata', icon = shiny::icon('table')),
      shinydashboard::menuItem("Etho backup manager", tabName = 'backup', icon = shiny::icon('save'))
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
