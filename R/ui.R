#' A UI for fslretho
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem
#' @importFrom shiny icon
#' @importFrom esquisse esquisserUI
#'
shinydashboard_ui <- function() {
  shinydashboardPlus::dashboardPagePlus(skin = "black",
    shinydashboardPlus::dashboardHeaderPlus(
      title = "FSLRetho2",
      left_menu = tagList(
        shinydashboardPlus::dropdownBlock(
          id = "scoringInput",
          title = "Scoring parameters",
          # icon =
          scoreDataUI("scoreData")
        ),
        shinydashboardPlus::dropdownBlock(
          id = "definedGroups",
          title = "Animal groups",
          # icon =
          defineGroupUI("defineGroup")
        ),

      ),
      tags$li(
        tags$div(id = 'dataset_title', class = 'mybox', style = "padding: 10px; border: black 2px solid",
          shiny::textOutput("dataset_name"),
        ), class = "dropdown user user-menu"

      )
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Welcome", tabName = 'welcome', icon = shiny::icon('info')),
        shinydashboard::menuItem("Load", tabName = 'load', icon = shiny::icon('upload')),
        shinydashboard::menuItem("Sleep analysis", tabName = 'sleep', icon = shiny::icon('moon')),
        shinydashboard::menuItem("Revise metadata", tabName = 'metadata', icon = shiny::icon('moon'))
      )
    ),
    # TODO Place somewhere the UI for scoreData
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = 'welcome', welcomePageUI()),
        shinydashboard::tabItem(tabName = 'load',
          tagList(
            loadDataUI("loadData-ethoscope", "ethoscope"),
            loadDataUI("loadData-dam", "dam")
          )
        ),
        shinydashboard::tabItem(tabName = "sleep",
          tabsetPanel(
            tabPanel(
              title = "esquisse",
              esquisse::esquisserUI(
                id = "esquisse",
                header = FALSE, # dont display gadget title
                choose_data = FALSE # dont display button to change data
              )
            ),
            tabPanel(
              title = "output",
              shiny::verbatimTextOutput("module_out")
            )
          )
        ),
        # shinydashboard::tabItem(tabName = 'sleep', analyseSleepUI("analyseSleep")),
        shinydashboard::tabItem(tabName = 'metadata', viewMetadataUI("viewMetadata"))
      )
    )
  )
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
