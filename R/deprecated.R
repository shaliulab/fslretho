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




parse_aesthetics <- function(data, mapping_list, color=NULL, fill=NULL, size=NULL) {
  # the entries in mapping_list will be passed to
  # ggplot transformed into a aesthetic object (call to aes.string)

  other_aesthetics <- list(color=color, fill=fill, size=size)

  for (i in 1:length(other_aesthetics)) {
    if (!is.null(other_aesthetics[[i]])) {
      aes_name <- names(other_aesthetics)[i]
      mapping_list[[aes_name]] <- other_aesthetics[[i]]
    }
  }

  # remove the variables where the value is ""
  # so aes_string does not complain
  aesthetics <- names(mapping_list)
  for (a in aesthetics) {
    if (mapping_list[[a]] == "") mapping_list[[a]] <- NULL
  }


  # make sure the mapping is possible using the provided data
  mapping_list <- lapply(mapping_list,
                         function(col){
                           if(col %in% colnames(data))
                             paste0("`", col, "`")
                           else
                             col
                         })
  return(mapping_list)

}


plotUI <- function(id) {

  ns <- NS(id)

  tagList(
    shiny::tabPanel(
      p("Aesthetics"),
      textInput(ns("color"), label = "Color by", value=NULL),
      textInput(ns("fill"), label = "Fill by", value=NULL),
      textInput(ns("size"), label = "Size by", value=NULL)
    ),
    shiny::tabPanel(
      p("Facet"),
      textInput(ns("facet"), label = "Facet by", value=NULL),
      textInput(ns("facet_row"), label = "Facet row by", value=NULL),
      textInput(ns("facet_col"), label = "Facet col by", value=NULL)
    ),
    wellPanel(
      actionButton(ns("button"), label = "Plot!"),
      plotOutput(ns("plot"))
    )
  )
}

plotServer <- function(id, input_rv, mapping) {

  moduleServer(
    id,
    function(input, output, session) {


      mapping_list_ <- make_labels(mapping)

      mapping <- reactive({
        mapping_list <- parse_aesthetics(input_rv$data, mapping_list_, color=input$color, fill=input$fill, size=input$size)
        mapping <- do.call(ggplot2::aes_string, mapping_list)
        mapping
      })

      p <- eventReactive(input$button, {
        d <- req(input_rv$data)

        if (isTruthy(input$facet) & !isTruthy(input$facet_row) & !isTruthy(input$facet_col)) {

          d$facet_row <- input$facet_row
          d$facet_col <- input$facet_row

          p <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2) +
            facet_grid(facet_row ~ facet_col)
          p
        }

        if (!isTruthy(input$facet) & isTruthy(input$facet_row) & isTruthy(input$facet_col)) {

          d$facet <- input$facet

          gg <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2) +
            facet_wrap("facet")

        } else {

          gg <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2)
        }
        gg

      }, ignoreInit = TRUE)

      output$plot <- renderPlot({
        p()
      })
    }
  )
}

# Old score module


#' Annotate behavior coming from DAM or ethoscope
#'
#' This function takes dependence on reactiveValues.
#' Thus it needs to be run on a reactive environment (observe/reactive)
#' @importFrom scopr annotate_all
#' @importFrom sleepr sleep_annotation sleep_dam_annotation
#' @param input_rv reactiveValues, incoming dataset with data, name and time slots
#' @param updateProgress function optionally taking a character that executes every time a new individual is processed
#' @param ... Additional arguments to scoring function
#' @export
score_monitor <- function(input_rv, FUN, updateProgress, ...) {

  SPARSE_DATA <- getOption("sparse_data", FALSE)
  CURATE <- ifelse(SPARSE_DATA, FALSE, TRUE)

  output_rv <- reactiveValues(
    data = NULL,
    name = NULL,
    time = NULL
  )

  data <- input_rv$data

  scoring_parameters <- list(...)

  if (SPARSE_DATA) {
    message("score module overrides time_window_length to 1 hour")
    scoring_parameters$time_window_length <- hours(1)
  }

  # Annotate all ROIS with all scoring FUN passed by the user
  args <- append(
    scoring_parameters,
    list(
      data = data,
      FUN = FUN,
      curate = CURATE,
      updateProgress = updateProgress
    )
  )

  if (DEBUG) message("Running behavioral annotation")
  data_annotated <- do.call(scopr::annotate_all, args)

  validate(need(nrow(data_annotated) > 0, "Data cannot be annotated. This could be due to your dataset being sparse"))
  output_rv$data <- data_annotated
  output_rv$name <- input_rv$name
  output_rv$time <- input_rv$time
  return(output_rv)
}


#' Shiny module to automatically score a raw behavr table
#'
#' Provide a multi-animal reactive behavr and return the scored version
#'
#' @param id Module id - character
#' @param input_rv reactiveValues with one slot per monitor
#' @param pb logical, if TRUE, a shiny progress bar will track the processing of each individual
#' @importFrom shiny moduleServer reactive observe eventReactive Progress
#' @importFrom behavr bin_apply_all
#' @importFrom rlang fn_fmls
scoreDataServer <- function(id, input_rv, pb=TRUE) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        ethoscope = reactiveValues(data  = NULL, name = NULL, time = NULL),
        dam = reactiveValues(data  = NULL, name = NULL, time = NULL)
      )

      progress_bar <- reactiveValues(progress = NULL, update = NULL)

      # Get just the annotation parameters
      scoring_parameters <- reactive(
        list(
          velocity_correction_coef = input$velocity_correction_coef,
          min_time_immobile = input$min_time_immobile,
          time_window_length = input$time_window_length
        )
      )

      # remove the parameters that the DAM scoring function does not need
      # it would error otherwise, Error in FUN: unused arguments
      # (velocity_correction_coef = 3e-04, time_window_length = 10, curate = TRUE)

      dam_scoring_parameters <- reactive({
        x <- scoring_parameters()
        x$time_window_length <- NULL
        x$curate <- NULL
        x$velocity_correction_coef <- NULL
        x
      })


      observeEvent(input_rv$dam$data, {
        req(input_rv$dam$data)
        n_individuals <- nrow(input_rv$dam$data[, meta = T])
        pb <- get_progress_bar(n_individuals, "Scoring")
        progress_bar$progress <- pb$progress
        progress_bar$update <- pb$update
      })


      FUN <- reactive({
        lapply(input$FUN, function(f) FUNCTION_MAP[[f]]$ethoscope)
      })

      # Score ethoscope
      observeEvent(c(input_rv$ethoscope$time, scoring_parameters(), FUN()), {

        req(input_rv$ethoscope$data)
        req(FUN())
        req(scoring_parameters())

        n_individuals <- nrow(input_rv$ethoscope$data[, meta = T])
        pb <- get_progress_bar(n_individuals, "Scoring")
        progress_bar$progress <- pb$progress
        progress_bar$update <- pb$update

        args <- append(list(input_rv=input_rv$ethoscope, updateProgress = progress_bar$update, FUN = FUN()), scoring_parameters())
        output_rv$ethoscope <- do.call(score_monitor, args)
        output_rv$ethoscope$time <- Sys.time()
        # just a sapply returns a matrix, so I need lapply and unlist
        output_rv$ethoscope$variables <- tryCatch(unlist(lapply(FUN(), function(f) {attr(f, "variables")()})), error = function(e) {"asleep"})
        on.exit(progress_bar$progress$close())
      }, ignoreInit = TRUE)

      # Score DAM
      observeEvent(c(input_rv$dam$time, scoring_parameters()), {
        req(input_rv$dam$data)
        output_rv$dam <- score_monitor(input_rv$dan, progress_bar$update, dam_scoring_parameters(), FUN=FUNCTION_MAP$dam)
        on.exit(progress_bar$progress$close())
      }, ignoreInit = TRUE)

      return(output_rv)
    })
}
