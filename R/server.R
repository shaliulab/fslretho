saveLoadServer <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {
      cache_dir <- file.path(
        FSLRethoConfiguration$new()$content[["folders"]][["ethoscope_cache"]][["path"]], "sessions"
      )

      observeEvent(input$save, {
        tosave <- shiny::reactiveValuesToList(data)

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

      return(saved_unified_data)
    }
  )

}



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


  # Save session
  # saved_unified_data <- callModule(
  #   module=saveLoadServer,
  #   id="saveLoadModule",
  #   data=unified_data
  # )

  # Load a past session
  observeEvent(input$`load-saveModule`, {
    unified_data$data <<- saved_unified_data()$data
    unified_data$name <<- saved_unified_data()$name
    unified_data$time <<- saved_unified_data()$time
  })

  # Everything below uses either unified_data or binned_data
  # This makes for a nice set of session saving objects
  binned_data <- binDataServer("binData", unified_data, main = TRUE)
  bout_data <- analyseBoutServer("analyseBout", unified_data)


  ## Metadata viz ----
  # View loaded metadata
  viewMetadataServer("viewMetadata", unified_data)

  rejoined_data <- reactiveValues(data=NULL, name=NULL, time=NULL)

  observeEvent(unified_data$time, {
    x <- rejoin_rv(binned_data$data)
    if (is.null(x$data)) x$data <- data.table(id = character(), asleep = logical())
    else rejoined_data$data <- x$data
    rejoined_data$name <- ifelse(is.null(x$name), "Empty", x$name)
    rejoined_data$time <- ifelse(is.null(x$time), 0, x$time)
  }, ignoreInit = FALSE)

  ## Plotting ----
  # shiny::callModule(
  #   module=sleepInteractionsServer,
  #   id="sleep_interactions",
  #   data=rejoined_data
  # )

  analyse_sleep_01 <- shiny::callModule(
    module = esquisse::esquisserServer,
    id = "analyseSleep_01",
    data = rejoined_data,
    dataModule = "GlobalEnv"
    # , launchOnStart=FALSE
  )



  # analyse_bout_01 <- callModule(
  #   module = esquisse::esquisserServer,
  # # analyse_bout_01 <- esquisse::esquisserServer(
  #   id = "analyseBout_01",
  #   data = rejoin_rv(bout_data$data),
  #   # dataModule = "GlobalEnv",
  #   dataModule = NULL,
  #   #input_modal = FALSE
  #
  # )

  output$analyseSleep_01_out <- renderPrint({
    req(binned_data$data)
    str(reactiveValuesToList(analyse_sleep_01))
  })

  # output$analyseBout_01_out <- renderPrint({
  #   req(bout_data$data)
  #   str(reactiveValuesToList(analyse_bout_01))
  # })


  output$dataset_name <- shiny::renderText({
    req(unified_data$name)
    paste0("Loaded dataset: ", unified_data$name)
  })

  observeEvent(input$about, {
    shiny::showModal(app_description())
  })


}