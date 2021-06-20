#' UI for the FSLRetho welcome page
#' @importFrom shiny fluidRow p
welcomePageUI <- function(id) {

  ns <- NS(id)

  documentation_url <- "https://docs.google.com/document/d/1cClaDaiQHbexiEVFpVo7bN-09ZgYSw-WLSYToWYuZU4"

  fluidPage(
    # style sheet
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="fslretho/css/styles.css"),
    ),

    titlePanel("FSLRetho2"),
      "Welcome to FSLRetho2, a web app designed to simplify the analysis of behavioral data",
      fluidRow(
        shinydashboard::box(title = "Show around the application",
               tags$video(id=ns("video1"), type = "video/mp4",src = "fslretho/static/fslretho2.h264", controls = "controls", width = "100%")
        )
        # another box would go here
      ),
      fluidRow(
        #another row would go here
      ),


      "A comprehensive documentation",
      a(href=documentation_url, target="_blank", "is available"),
      br(),
      actionLink(
        inputId = ns("about"), label = "About", icon = NULL,
        # tags$img(src = "logo_dreamRs_couleur.png", style = "width: 50px; float:left; margin-right: 10px;"),
        style = "color: #112446; padding: 5px; line-height:25px;", class = "pull-right"
      )
  )
}





welcomePageServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      modal <- modalDialog(
        title = "About this application",
        tags$div(
          tags$b("The packages :"),
          tags$ul(
            tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/"), "for the application."),
            tags$li(tags$a("esquisse", href = "https://dreamrs.github.io/esquisse/index.html"), "for the graphical interface to ggplot2 functionality"),
            tags$li(
              tags$a("shinydashboard", href = "https://dreamrs.github.io/esquisse/index.html"),
              "and",
              tags$a("shinydashboardPlus", href = "https://rinterface.com/shiny/shinydashboardPlus/"),
              "for customization of the GUI",
            ),
            tags$li(
              tags$a("rlang", href = "https://rlang.r-lib.org/"), "for metaprogramming in R"
            ),
            tags$li(
              tags$a("data table", href = "https://rdatatable.gitlab.io/data.table/"), "and", tags$a("dplyr", href = "https://dplyr.tidyverse.org/"),
              "for data manipulation"
            ),
            tags$li(
              "The rethomics suite of R packages, originally available under",
              tags$a("rethomics", href = "https://rethomics.github.io/"),
              "and composed by: ",
              tags$a("behavr", href = "https://github.com/rethomics/behavr/"), ",",
              tags$a("scopr", href = "https://github.com/rethomics/scopr/"), ",",
              tags$a("sleepr", href = "https://github.com/rethomics/sleepr/"), ",",
              tags$a("ggetho", href = "https://github.com/rethomics/ggetho/"), "and",
              tags$a("zeitgebr", href = "https://github.com/rethomics/zeitgbr/"),
              "with added features as stored in ",
              tags$a("shaliulab github", href = "https://github.com/shaliulab/"),
              "for the backend providing the core behavioral analysis functionality"
            )
          ),
          tags$b("The authors :"),
          tags$ul(
            tags$li(
              "Developed at the Sha Liu Lab @ KU Leuven-VIB Center for Brain and Disease Research in Leuven, Belgium."
            ),
            tags$a(
              class = "btn btn-default", icon("github"), "antortjim",
              href = "https://github.com/antortjim", style = "background-color: #1DA1F2; color: #FFF;"
            )
          )
        )
      )
      observeEvent(input$about, {
        showModal(modal)
      })
    })
}