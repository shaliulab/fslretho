#include "configuration.R"
get_sessions <- function() {
  cache_dir <- file.path(
    FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]], "sessions"
  )

  fslretho_sessions <- list.files(path = cache_dir)
  names(fslretho_sessions) <- fslretho_sessions %>% sapply(., function(x) substr(x, 1, 10))
  fslretho_sessions <- as.list(fslretho_sessions)
  fslretho_sessions <- ifelse(length(fslretho_sessions) == 0, list("Empty_cache" = ""), fslretho_sessions)

  return(fslretho_sessions)
}

#' @importFrom shinydashboard menuItem dashboardSidebar
#' @importFrom shiny icon
get_sidebar <- function() {
  shinydashboard::dashboardSidebar(
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
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem dashboardBody
#' @importFrom shiny icon
#' @importFrom esquisse esquisserUI
#' @importFrom magrittr `%>%`
#' @importFrom shinybusy add_busy_bar
shinydashboard_ui <- function() {

  header <- get_header()
  sidebar <- get_sidebar()
  body <- get_body()

ui <- shinydashboardPlus::dashboardPage(skin = "black",
    header,
    sidebar,
    body
  )
  ui
}

#' @importFrom shinythemes shinytheme
#' @import shiny
navbar_ui <- function() {

  ui = shiny::navbarPage(
    "FSLRetho2", theme = shinythemes::shinytheme("flatly"),
    shiny::tabPanel(
      "Welcome",
      welcomePageUI()
    ),

    shiny::tabPanel(
      "Load metadata",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          scoreDataUI("scoreData")
        ),
        shiny::mainPanel(
          shiny::tagList(loadDataUI("loadData-ethoscope", "ethoscope"), loadDataUI("loadData-dam", "dam"))
        )
      )
    ),

    shiny::tabPanel(
      "Sleep analysis",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          scoreDataUI("scoreData")
        ),
        shiny::mainPanel(
          analyseSleepUI("analyseSleep")
        )
      )
    )

  )
}

ui <- shinydashboard_ui
