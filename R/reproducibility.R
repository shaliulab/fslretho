#' Return a script that reproduces the fslretho results
#' Ideally this would be done as in esquisse with metaprogramming
#' but this is good enoug for now
#' @param params reactiveValues with parameters relevant for the analysis
#' @params envir the environment for which to look up the analysis parameters
#' @importFrom rlang env
reproducibilityModuleServer <- function(id,  envir = rlang::env()) {
# reproducibilityModuleServer <- function(id,  envir = parent.frame()) {

 moduleServer(
   id,
   function(input, output, session) {


      params <- reactiveValues(
         # load module
         result_dir = NULL,
         # score module
         velocity_correction_coef = NULL,
         time_window_length = NULL,
         min_time_immobile = NULL,
         # bin module
         summary_time_window = NULL,
         summary_FUN = NULL
      )

      observeEvent(envir$selected_data$time, {
         result_dir <- req(envir$input$`loadData-result_dir_ethoscope`)
         params$result_dir <- result_dir
      }, ignoreInit = TRUE)


      observeEvent(envir$scored_data$ethoscope$time, {
         params$velocity_correction_coef <- envir$input$`scoreData-velocity_correction_coef`
         params$time_window_length <- envir$input$`scoreData-time_window_length`
         params$min_time_immobile <- envir$input$`scoreData-min_time_immobile`
      }, ignoreInit = TRUE)

      observeEvent(envir$sleep_data$time, {
         params$summary_time_window <- envir$input$`sleepBin-summary_time_window`
         params$summary_FUN <- envir$input$`sleepBin-summary_FUN`
      }, ignoreInit = TRUE)

     script <- reactive({

       conf <- fslretho::FSLRethoConfiguration$new()
       NCORES <- conf$content$scopr$ncores
       CACHE <- conf$content$scopr$folders$cache$path
       VERBOSE <- TRUE
       TESTING <- conf$content$testing
       DEBUG <- conf$content$debug
       # params <- list(result_dir = "/ethoscope_data/results/", velocity_correction_coef = 0.0048, time_window_length = 10, min_time_immobile = 300, summary_time_window = 30, summary_FUN = "mean")
       lines <- list(
         "# Install behavioral analysis backend",
         "# please mind these libraries are modified versions of https://github.com/rethomics",
         "# reproducibility is not guaranteed if installing from https://github.com/rethomics",
         "# instead install like this",
         "# devtools::install_github('shaliulab/behavr@deployment')",
         "# devtools::install_github('shaliulab/scopr@deployment')",
         "# devtools::install_github('shaliulab/sleepr@deployment')",
         "# devtools::install_github('shaliulab/ggetho@deployment')",
         "# data.table is a behavr dependency",
         "# ggplot2 is a ggetho dependency",
         "# both will be loaded when we load behavr and ggetho respectively,",
         "# but it is still nice to load them separately just to show they are used separately",
         "library(data.table)",
         "library(ggplot2)",
         "library(behavr)",
         "library(scopr)",
         "library(sleepr)",
         "library(ggetho)",
         "# metadata_path <- PATH TO YOUR METADATA.csv",
         "## ---- Load metadata to R and validate to avoid typical errors",
         "monitor <- fslretho::get_monitor_name(metadata_path)",
         "metadata <- fslretho::load_metadata(metadata_path, monitor)",
         "validate_metadata(metadata, monitor)",
         "if (monitor == 'ethoscope') {",
         "  # Link metadata to a local sqlite3 database",
         paste0("  ", "metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = '", params$result_dir, "')"),
         "  ## ---- Load data to R. This may take a while depending on your dataset size.",
         "  # Future runs with the same data will take less because the result is cached",
         "  dt_raw <- scopr::load_ethoscope(",
         "    metadata = metadata,",
         "    # this means the reference_hour is taken from the metadata, animal by animal",
         "    # please make sure there is a column called reference_hour in your metadata!",
         "    reference_hour = NA,",
         paste0("    ", "ncores = ", NCORES, ","),
         paste0("    ", "cache = '", CACHE, "',"),
         paste0("    ", "verbose = ", VERBOSE),
         "  )",
         "  ## ---- Annotate or summarise the behavior",
         "  args <- list(",
         paste0("    velocity_correction_coef = ", params$velocity_correction_coef, ","),
         paste0("    time_window_length = ", params$time_window_length, ","),
         paste0("    min_time_immobile = ", params$min_time_immobile, ","),
         "    data = dt_raw,",
         "    # Add other functions inside the list() statement if you wish!",
         "    # see TODO",
         "    FUN = list(sleepr::sleep_annotation),",
         "    curate = TRUE",
         "  )",
         "  dt <- do.call(scopr::annotate_all, args)",
         "  ## ---- Quantify fraction of time spent in the sleep state",
         "  # This is done in non overlapping windows of 30 minutes",
         "  bout_analysis <- F # set to TRUE if you want to do bout analysis",
         "  if (! bout_analysis) {",
         "    preproc_data <- dt",
         "    y <- 'asleep'",
         "  } else {",
         "    preproc_data <- fslretho::bout_analysis(data=dt, var='asleep')",
         "    y <- 'duration'",
         "  }",
         "  binned_dataset <- behavr::bin_apply_all(",
         "    preproc_data,",
         "     x = 't',",
         "    y = y,",
         paste0("    ", "x_bin_length = behavr::mins(", params$summary_time_window, "),"),
         paste0("    FUN = match.fun('", params$summary_FUN, "')"),
         ")",
         "  ## ---- Pareto rule for SD",
         "  # Run this block of code if you want to apply a QC filter that makes sure that",
         "  # SD'd flies are really SD: a wake fly is not just moving, but it should not spend",
         "  more than 80% of the time in less than 20% of the tube",
         "  ",
         "  ## ---- Plot",
         "  # This code generates the classic sleep amount trace plot",
         "  # Please look up in the documentation for other plots",
         "  data <- behavr::rejoin(binned_dataset)",
         "  gg <- ggplot(",
         "    # load the dataset into ggplot2",
         "    data,",
         "    # map time to the X axis and the sleep amount to the Y axis",
         "      aes(x = t, y = asleep)",
         "    ) +",
         "    # line plot showing the fraction of time spent sleep each 30 mins",
         "    # and a shade representing the standard deviation",
         "    # (if more than 1 animal is shown in the same panel, remove the facet_wrap call)",
         "    ggetho::geom_pop_etho() +",
         "    # mark whether the light is on (L) or off (D)",
         "    ggetho::stat_ld_annotations(color=NA, height=1, alpha=0.2) +",
         "    # show time in hours",
         "    ggetho::scale_x_hours() +",
         "    # produce a separate panel for each animal",
         "    facet_wrap('id')",
         "    gg",
         "}",
         "data.table::fwrite(x = data, 'binned_dataset.csv')",
         "data.table::fwrite(x = dt, 'scored_dataset.csv')",
         "data.table::fwrite(x = dt_raw, 'raw_dataset.csv')",
         "ggplot2::ggsave(plot = gg, 'plot.png', height = 10, width = 20)"
       )

       script <- paste(lines, collapse = "\n")
     })

     output$script <- renderPrint({
        cat(script())
     })

     output$download <- downloadHandler(
        filename = function() {
           "script.R"
        },
        content = function(file) {
           write(x = script(), file = file)
        }
     )
  })
}

reproducibilityModuleUI <- function(id) {

  ns <- NS(id)
  tagList(
     downloadButton(ns("download")),
     verbatimTextOutput(ns("script"))
  )
}