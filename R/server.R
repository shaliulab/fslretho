#' Server function of FSLRetho
#'
#' @import shiny
#' @importFrom shinylogs track_usage store_json
#' @importFrom esquisse esquisserServer
#' @importFrom fslsleepr bout_analysis
#' @noRd
server <- function(input, output, session) {

  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  # Placeholder where to keep the loaded data and its name
  raw_data <- reactiveValues(data = NULL, name = NULL)

  ethoscope_data <- loadEthoscopeServer("loadData-ethoscope")
  dam_data <- loadDamServer("loadData-dam")

  observeEvent(ethoscope_data$time, {
    raw_data <- update_rv(raw_data, ethoscope_data)
  }, ignoreInit = TRUE)

  observeEvent(dam_data$time, {
    raw_data <- update_rv(raw_data, dam_data)
  }, ignoreInit = TRUE)


  # bind the content of rv to the last modified module_data
  # raw_data <- watch_input(rv, ethoscope_data, dam_data)

  scored_data <- scoreDataServer("scoreData", raw_data)
  # grouped_data <- defineGroupServer("defineGroup", scored_data)

  viewMetadataServer("viewMetadata", scored_data)

  binned_data <- binDataServer("binData", scored_data)
  bout_data <- analyseBoutServer("analyseBout", scored_data)

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
    input_modal = FALSE
  )

  analyse_bout <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseBout",
    data = rejoin_rv(bout_data),
    dataModule = NULL,
    input_modal = FALSE
  )

  output$analyseSleep_01_out <- renderPrint({
    req(binned_data$data)
    str(reactiveValuesToList(analyse_sleep_01))
  })

  output$analyseSleep_02_out <- renderPrint({
    req(binned_data$data)
    str(reactiveValuesToList(analyse_sleep_02))
  })


  output$analyseBout_out <- renderPrint({
    req(bout_data$data)
    str(reactiveValuesToList(analyse_bout))
  })

  output$dataset_name <- shiny::renderText({
    req(scored_data$name)
    paste0("Loaded dataset: ", scored_data$name)
  })

}