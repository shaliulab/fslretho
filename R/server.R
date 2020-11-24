#' Server function of FSLRetho
#'
#' @import shiny
#' @importFrom shinylogs track_usage store_json
#' @importFrom esquisse esquisserServer
#' @importFrom fslsleepr bout_analysis
#' @importFrom ggplot2 ggplot facet_wrap aes facet_grid
#' @importFrom fslggetho stat_ld_annotations stat_pop_etho
#' @importFrom cowplot plot_grid
#' @importFrom rlang expr
#' @import fslbehavr
#' @noRd
server <- function(input, output, session) {

  ## Preparation ----
  # Run a simple ethoscope backup manager
  backupManagerServer("manageBackup")

  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  options("retry_locked" = tempfile(pattern = "retry_locked"))
  message(sprintf("retry_locked -> %s", getOption("retry_locked"))


  # Define a trigger shared across modules
  reload <- reactive({
    req(!is.null(input$reloadData))
    input$reloadData
  })

  ## Analysis ----
  # Load and analyse data provided by user
  raw_data <- loadDataServer("loadData", reload)
  scored_data <- scoreDataServer("scoreData", raw_data)
  unified_data <- unify_datasets(id = "", scored_data)


  cache_dir <- file.path(
    FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]],
    "sessions"
  )

  observeEvent(input$save, {

    tosave <- shiny::reactiveValuesToList(unified_data)

    saveRDS(
      object = tosave,
      file = file.path(cache_dir, input$rds_save)
    )
  })

  saved_unified_data <- reactive({
    readRDS(
      file = file.path(cache_dir, input$rds_load)
    )
  })

  observeEvent(input$load, {
    unified_data$data <<- saved_unified_data()$data
    unified_data$name <<- saved_unified_data()$name
    unified_data$time <<- saved_unified_data()$time
  })

  # Everything below uses either unified_data or binned_data
  # This makes for a nice set of session saving objects
  binned_data <- binDataServer("binData", unified_data, main = TRUE)
  bout_data <- analyseBoutServer("analyseBout", unified_data)

  # TODO Make this nicer
  preprocessing <- reactiveValues(data = NULL)

  observe({

    req(binned_data$name)
    req(binned_data$data)
    req(binned_data$y)
    req(binned_data$summary_FUN)
    keep_columns <- setdiff(colnames(binned_data$data[, meta=TRUE]), c("t", "id"))

    sleep_expression <- rlang::expr(fslbehavr::bin_all(data = !!rlang::sym(binned_data$name), y = !!binned_data$y, x = "t",
                                                      x_bin_length = !!fslbehavr::days(28),
                                                      FUN = !!binned_data$summary_FUN, keep_columns = !!keep_columns))

    bout_expression <- rlang::expr(fslbehavr::bin_all(data = !!rlang::sym(binned_data$name), y ="duration", x = "t",
                                     x_bin_length = !!fslbehavr::days(28),
                                     FUN = !!binned_data$summary_FUN, keep_columns = !!keep_columns))

    preprocessing$sleep <- sleep_expression
    preprocessing$bout <- bout_expression
  })

  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", unified_data)

  ## Plotting ----
  # TODO Put this in its own module
  analyse_sleep_00 <- reactiveVal(NULL)
  output$analyseSleep_00 <- renderPlot({
    input$refresh_analyseSleep_00
    analyse_sleep_00()
  })

  observe({
    req(binned_data$data$t)
    req(binned_data$data$asleep)
    req(binned_data$data[, meta = T]$region_id)
    data <- fslbehavr::rejoin(binned_data$data)
    # saveRDS(object = data, file = "/tmp/data.rds")

    sleep_trace <- ggplot2::ggplot(data = data, ggplot2::aes(x = t, y = asleep)) +
      fslggetho::stat_pop_etho() +
      fslggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
      ggplot2::facet_grid(region_id ~ .)

    if (isTruthy(binned_data$data$interactions)) {
      interactions_trace <- ggplot2::ggplot(data = data, ggplot2::aes(x = t, y = interactions)) +
        fslggetho::stat_pop_etho() +
        fslggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA)  +
        ggplot2::facet_grid(region_id ~ .)

      output_plot <- cowplot::plot_grid(sleep_trace, interactions_trace, ncol = 2)
    } else {
      output_plot <- sleep_trace
    }

    analyse_sleep_00(output_plot)

  })

  analyse_sleep_01 <- callModule(
    module = esquisse::esquisserServer,
  # analyse_sleep_01 <- esquisse::esquisserServer(
    id = "analyseSleep_01",
    data = rejoin_rv(binned_data),
    # dataModule = "GlobalEnv",
    dataModule = NULL,
    input_modal = FALSE
  )

  analyse_sleep_02 <- callModule(
    module = esquisse::esquisserServer,
  # analyse_sleep_02 <- esquisse::esquisserServer(
    id = "analyseSleep_02",
    data = rejoin_rv(binned_data),
    # dataModule = "GlobalEnv",
    dataModule = NULL,
    input_modal = FALSE,
    preprocessing_expression = preprocessing$sleep
  )

  analyse_bout_01 <- callModule(
    module = esquisse::esquisserServer,
  # analyse_bout_01 <- esquisse::esquisserServer(
    id = "analyseBout_01",
    data = rejoin_rv(bout_data),
    # dataModule = "GlobalEnv",
    dataModule = NULL,
    input_modal = FALSE

  )

  analyse_bout_02 <- callModule(
    module = esquisse::esquisserServer,
  # analyse_bout_02 <- esquisse::esquisserServer(
    id = "analyseBout_02",
    data = rejoin_rv(bout_data),
    # dataModule = "GlobalEnv",
    dataModule = NULL,
    input_modal = FALSE,
    preprocessing_expression = preprocessing$bout

  )

  output$analyseSleep_01_out <- renderPrint({
    req(binned_data$data)
    str(reactiveValuesToList(analyse_sleep_01))
  })

  output$analyseSleep_02_out <- renderPrint({
    req(binned_data$data)
    str(reactiveValuesToList(analyse_sleep_02))
  })


  output$analyseBout_01_out <- renderPrint({
    req(bout_data$data)
    str(reactiveValuesToList(analyse_bout_01))
  })


  output$analyseBout_02_out <- renderPrint({
    req(bout_data$data)
    str(reactiveValuesToList(analyse_bout_02))
  })

  output$dataset_name <- shiny::renderText({
    req(unified_data$name)
    paste0("Loaded dataset: ", unified_data$name)
  })

  observeEvent(input$about, {
    shiny::showModal(app_description())
  })


}