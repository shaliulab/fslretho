#' UI for the FSLRetho welcome page
#' @importFrom shiny fluidRow p
welcomePageUI <- function() {

  documentation_url <- "https://docs.google.com/document/d/1cClaDaiQHbexiEVFpVo7bN-09ZgYSw-WLSYToWYuZU4"

  shiny::fluidPage(
    # style sheet
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="fslretho/css/styles.css"),
    ),

    shiny::titlePanel("FSLRetho2"),
      "Welcome to FSLRetho2, a web app designed to simplify the analysis of behavioral data",
      fluidRow(
        shinydashboard::box(title = "Show around the application",
               tags$video(id="video1", type = "video/mp4",src = "fslretho/static/fslretho2.h264", controls = "controls", width = "100%")
        )
        # another box would go here
      ),
      fluidRow(
        #another row would go here
      ),


      "A comprehensive documentation",
      shiny::a(href=documentation_url, target="_blank", "is available"),
      br(),
      shiny::actionLink(
        inputId = "about", label = "About", icon = NULL,
        # tags$img(src = "logo_dreamRs_couleur.png", style = "width: 50px; float:left; margin-right: 10px;"),
        style = "color: #112446; padding: 5px; line-height:25px;", class = "pull-right"
      )
  )
}