app_description <- function() {
  modalDialog(
    title = "About this application",
    tags$div(
      tags$b("The packages :"),
      tags$ul(
        tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/"), "for the application."),
        tags$li(tags$a("rethomics", href = "https://rethomics.github.io/"), "for the backend of the behavioral analysis functions"),
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
}

common_help <- function() {
  tags$div(
    tags$b(
      "Pause the generation and rendering of the plot in the website while you build it
      by toggling the Play/Pause button on the top right corner"
    ),
    tags$ul(
      tags$b("Change the organization of the plot by mapping extra columns of your data to aesthetical elements of the plot"),
      tags$li(
        "Facets allow you to generate one plot for each group of animals sharing the value of a particular metavariable,
        tipically genotype, sex, treatment, etc, You can define them by dragging the variable of interest into any of the facet boxes"
          ),
      tags$li(
        "Drag a metavariable to the group box to create two subsets of your data
        and map a separate graph to each"
      ),
      tags$li(
        "Drag a metavariable to the color box to create two subsets of your data and map a separate graph
        to each where each graph gets a different edge color"
      ),
      tags$li(
        "Drag a metavariable to the fill box to create two subsets of your data and map a separate graph
        to each where each graph gets a different filling color"
      ),
      tags$li(
        "Drag a metavariable to the size box to create two subsets of your data and map a separate graph
        to each where each graph gets a different size"
      )
    ),
    tags$b("Change displayed data"),
      tags$ul(
        tags$li(
          "Keep only a subset of your data for plotting by filtering rows that dont match the requirements
          specified with the controls in the Data tab")
      ),
    tags$b("Change plot appearance"),
      tags$ul(
        tags$li("Change theme, color scale, ..., with the controls in Labels & Title and Plot options")
      ),
    tags$p(tags$b("Download the plotted data (with filters applied)"), "by clicking on Export & code > .csv"),
    tags$p(tags$b("Download the raw data (before filters are applied"), "by clicking on Export & code > raw.csv"),
    tags$p(tags$b("Download the plot in .png format by clicking on Export & code > .png"))
  )
}

analyse_sleep_01_help <- function() {
  tags$div(
    "Visualize sleep time series. Drag t to the X box and asleep to the Y box
     to obtain a plot showing a sleep statistic over time for all animals in your dataset.
     If the selected statistic is the mean, the plot shows the sleep amount over time.
     You may be interested in using other statistics such as P_doze or P_wake.",
    common_help()
  )
}

analyse_sleep_02_help <- function() {
  tags$div(
    "Summarise sleep time series of each individual by marginalizing time.
    Here you can generate plots that show a summary over the selected time.
    For instance, you could create a boxplot of sleep amount for group(s) of flies by mapping
    asleep to Y, genotype (to have a separate boxplot for each genotype) to X and selecting boxplot in the geom selector in the top right corner
    ",
    common_help()
  )
}
