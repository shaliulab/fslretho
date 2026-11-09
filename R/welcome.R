DOCUMENTATION_URL <- "https://docs.google.com/document/d/1cClaDaiQHbexiEVFpVo7bN-09ZgYSw-WLSYToWYuZU4"

#' Link that opens in a new tab, so the user does not leave (and lose) the app
#' @noRd
external_link <- function(label, href) tags$a(label, href = href, target = "_blank", rel = "noopener")

welcomePageUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 7,
      shinydashboard::box(
        width = NULL, title = "Welcome to FSLRetho", status = "primary", solidHeader = TRUE,
        tags$p("FSLRetho analyses the sleep and circadian rhythm of flies recorded with ethoscopes."),
        tags$ol(
          tags$li(tags$b("Load"), ": upload a metadata file describing your flies."),
          tags$li(tags$b("Sleep / Bout analysis"), ": plot the fraction of time asleep and the sleep bouts."),
          tags$li(tags$b("Period analysis"), ": compute periodograms and spectrograms."),
          tags$li(tags$b("Download datasets"), " (top bar): export any of the tables as .csv.")
        ),
        tags$p(
          "New here? Read the ", actionLink(ns("go_help"), "Help"), " tab, which explains every step and parameter. ",
          "A longer guide is ", external_link("available online", DOCUMENTATION_URL), "."
        ),
        actionLink(ns("about"), "About", class = "pull-right")
      )
    ),
    column(
      width = 5,
      shinydashboard::box(
        width = NULL, title = "Tour of the application",
        # recorded with inst/tour/record_tour.js
        tags$video(src = "fslretho/static/fslretho3-tour.mp4", type = "video/mp4", controls = "controls", width = "100%")
      )
    )
  )
}

welcomePageServer <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input$go_help, shinydashboard::updateTabItems(parent_session, "tabs", "help"))

      observeEvent(input$about, {
        showModal(modalDialog(
          title = "About this application", easyClose = TRUE,
          tags$b("Built with"),
          tags$ul(
            tags$li(external_link("shiny", "https://shiny.posit.co/"), "for the application,"),
            tags$li(external_link("esquisse", "https://dreamrs.github.io/esquisse/"), "for the graphical interface to ggplot2,"),
            tags$li(
              external_link("shinydashboard", "https://rstudio.github.io/shinydashboard/"), "and",
              external_link("shinydashboardPlus", "https://rinterface.github.io/shinydashboardPlus/"), "for the layout,"
            ),
            tags$li(
              external_link("data.table", "https://rdatatable.gitlab.io/data.table/"), "and",
              external_link("dplyr", "https://dplyr.tidyverse.org/"), "for data manipulation,"
            ),
            tags$li(
              "the ", external_link("rethomics", "https://rethomics.github.io/"), " packages ",
              external_link("behavr", "https://github.com/rethomics/behavr"), ", ",
              external_link("scopr", "https://github.com/rethomics/scopr"), ", ",
              external_link("sleepr", "https://github.com/rethomics/sleepr"), ", ",
              external_link("ggetho", "https://github.com/rethomics/ggetho"), " and ",
              external_link("zeitgebr", "https://github.com/rethomics/zeitgebr"),
              ", with the changes kept at ", external_link("github.com/shaliulab", "https://github.com/shaliulab"), "."
            )
          ),
          tags$b("Authors"),
          tags$p(
            "Developed at the Sha Liu Lab, KU Leuven-VIB Center for Brain and Disease Research, Leuven, Belgium.",
            external_link(tagList(icon("github"), "antortjim"), "https://github.com/antortjim")
          )
        ))
      })
    }
  )
}
