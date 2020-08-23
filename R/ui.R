#' A UI for fslretho
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem
#' @importFrom shiny icon
#' @importFrom esquisse esquisserUI
#' @importFrom magrittr `%>%`
#' @importFrom shinybusy add_busy_bar
shinydashboard_ui <- function() {


  if (FSLRethoConfiguration$new()$content$debug) {
    browserButton <- actionButton("browser", label = "Browser")
  } else {
    browserButton <- tags$div(style = "hidden")
  }

  ui <- shinydashboardPlus::dashboardPagePlus(skin = "black",

    shinydashboardPlus::dashboardHeaderPlus(
      title = "FSLRetho2",
      left_menu = tagList(
        browserButton,
        shinydashboardPlus::dropdownBlock(
          id = "scoringInput",
          title = "Scoring parameters",
          # icon =
          scoreDataUI("scoreData")
        ),
        shinydashboardPlus::dropdownBlock(
          id = "binningInput",
          title = "Binning parameters",
          # icon =
          binDataUI("binData")
        ),
      ),
      tags$li(
        shiny::actionButton("reloadData", "", icon = icon("redo")),
        class = "dropdown user user-menu"
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
        shinydashboard::menuItem("Bout analysis", tabName = 'bout', icon = shiny::icon('moon')),
        shinydashboard::menuItem("Revise metadata", tabName = 'metadata', icon = shiny::icon('table')),
        shinydashboard::menuItem("Etho backup manager", tabName = 'backup', icon = shiny::icon('save'))
      )
    ),
    # TODO Place somewhere the UI for scoreData
    shinydashboard::dashboardBody(
      shinybusy::add_busy_bar(color = "#FF0000"),

      tags$link(rel = "stylesheet", type = "text/css", href = "fslretho/css/styles.css"),
      tags$script(src = "fslretho/js/my_javascript.js"),

      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = 'welcome', welcomePageUI()),
        shinydashboard::tabItem(tabName = 'load',
          tagList(
            loadDataUI("loadData-ethoscope", "ethoscope"),
            loadDataUI("loadData-dam", "dam")
          )
        ),
        shinydashboard::tabItem(tabName = "sleep",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Sleep trace + interactions", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = "90%",
              plotOutput("analyseSleep_00", height = "400px") %>%
                tagAppendAttributes(class = "resizable") %>%
                tagAppendAttributes(onmouseup = 'refresh_plot()')
            )
          ),

          analyse_sleep_01_help(),

          tabsetPanel(
            tabPanel(
              title = "Plot",
              esquisse::esquisserUI(
                id = "analyseSleep_01",
                header = FALSE, # dont display gadget title
                choose_data = FALSE, # dont display button to change data
                disable_filters = FALSE
              )
            ),
            tabPanel(
              title = "Output",
              shiny::verbatimTextOutput("analyseSleep_01_out")
            )
          ),

          analyse_sleep_02_help(),


          tabsetPanel(
            tabPanel(
              title = "Plot",
              esquisse::esquisserUI(
                id = "analyseSleep_02",
                header = FALSE, # dont display gadget title
                choose_data = FALSE, # dont display button to change data
                disable_filters = FALSE
              )
            ),
            tabPanel(
              title = "Output",
              shiny::verbatimTextOutput("analyseSleep_02_out")
            )
          )
        ),
        shinydashboard::tabItem(tabName = "bout",
          tabsetPanel(
            tabPanel(
              title = "Bout analysis",
              esquisse::esquisserUI(
                id = "analyseBout_01",
                header = FALSE, # dont display gadget title
                choose_data = FALSE, # dont display button to change data
                disable_filters = FALSE
              )
            ),
            tabPanel(
              title = "bout output",
              shiny::verbatimTextOutput("analyseBout_01_out")
            )
          ),
          tabsetPanel(
            tabPanel(
              title = "Bout analysis",
              esquisse::esquisserUI(
                id = "analyseBout_02",
                header = FALSE, # dont display gadget title
                choose_data = FALSE, # dont display button to change data
                disable_filters = FALSE
              )
            ),
            tabPanel(
              title = "bout output",
              shiny::verbatimTextOutput("analyseBout_02_out")
            )
          )

        ),

        shinydashboard::tabItem(tabName = 'metadata', viewMetadataUI("viewMetadata")),
        shinydashboard::tabItem(tabName = 'backup', backupManagerUI("manageBackup"))
      )
    )
  )
  ui
  # browser()
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