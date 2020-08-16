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

  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  # Placeholder where to keep the loaded data and its name
  raw_data <- reactiveValues(
    data = NULL,
    name = NULL,
    time = NULL
  )

  reload <- reactive({
    input$reloadData
  })

  ethoscope_data <- loadEthoscopeServer("loadData-ethoscope", reload)
  dam_data <- loadDamServer("loadData-dam", reload)


  observeEvent(ethoscope_data$time, {
    raw_data <- update_rv(raw_data, ethoscope_data)
  }, ignoreInit = TRUE)

  observeEvent(dam_data$time, {
    raw_data <- update_rv(raw_data, dam_data)
  }, ignoreInit = TRUE)


  # bind the content of rv to the last modified module_data
  # raw_data <- watch_input(rv, ethoscope_data, dam_data)

  scored_data <- scoreDataServer("scoreData", raw_data)

  viewMetadataServer("viewMetadata", scored_data)

  binned_data <- binDataServer("binData", scored_data, main = TRUE)
  bout_data <- analyseBoutServer("analyseBout", scored_data)

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

    sleep_trace <- ggplot2::ggplot(data = fslbehavr::rejoin(binned_data$data), ggplot2::aes(x = t, y = asleep)) +
      fslggetho::stat_pop_etho() +
      fslggetho::stat_ld_annotations(height = 1, alpha = 0.2, color = NA) +
      ggplot2::facet_grid(region_id ~ .)

    if (isTruthy(binned_data$data$interactions)) {
      interactions_trace <- ggplot2::ggplot(data = fslbehavr::rejoin(binned_data$data), ggplot2::aes(x = t, y = interactions)) +
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
    id = "analyseSleep_01",
    data = rejoin_rv(binned_data),
    dataModule = NULL,
    input_modal = FALSE
  )

  analyse_sleep_02 <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseSleep_02",
    data = rejoin_rv(binned_data),
    dataModule = NULL,
    input_modal = FALSE,
    preprocessing_expression = preprocessing$sleep
  )

  analyse_bout_01 <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseBout_01",
    data = rejoin_rv(bout_data),
    dataModule = NULL,
    input_modal = FALSE

  )

  analyse_bout_02 <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseBout_02",
    data = rejoin_rv(bout_data),
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
    req(scored_data$name)
    paste0("Loaded dataset: ", scored_data$name)
  })

  observeEvent(input$about, {
    shiny::showModal(app_description())
  })


}