#' Server function of FSLRetho
#'
#' @import shiny
#' @importFrom shinylogs track_usage store_json
#' @importFrom zeallot `%<-%`
#' @importFrom esquisse esquisserServer
#' @noRd
server <- function(input, output, session) {


  # Log relevant events made by the user
  shinylogs::track_usage(storage_mode = shinylogs::store_json(path = "logs/"))

  last_monitor <- shiny::reactiveVal("ethoscope")
  dataset_name <- shiny::reactiveVal("NONE")
  apply_filter <- shiny::reactiveVal(0)

  output$dataset_name <- shiny::renderText({
    paste0("Loaded dataset: ", dataset_name())
  })

  message("Point 1")

  raw_data_multiple <- list(
    "ethoscope" = loadEthoscopeServer("loadData-ethoscope", last_monitor, dataset_name),
    "dam" = loadDamServer("loadData-dam", last_monitor, dataset_name)
  )
  message("Point 2")

  # select data slot
  raw_data <- reactive({

    if (dataset_name() != "NONE") {
      dataset_name()
      last_monitor()
      message(sprintf("Updating raw_data slot with monitor %s, dataset %s", last_monitor(), dataset_name()))
      raw_data_multiple[[last_monitor()]]()
    } else {
      fslbehavr::toy_ethoscope_data()
    }
  })

  scored_data <- scoreDataServer("scoreData", raw_data, dataset_name, apply_filter, last_monitor)

  groups <- defineGroupServer("defineGroup", scored_data, apply_filter)

  # FIXME
  refresh_plot <- reactive({
    dataset_name()
    apply_filter()
    last_monitor()
    scored_data$data()
  })


  grouped_data <- reactive({
    set_groups(scored_data, groups, refresh_plot)
  })


  viewMetadataServer("viewMetadata", grouped_data)

  data_r <- reactiveValues(
    data = reactive(NULL),
    metadata = reactive(NULL),
    name = reactive(""),
    extra = reactive(
        list(
        scale_X_FUN = NULL,
        discrete_y = FALSE
      )
    )
  )


  observeEvent(refresh_plot(), {
    #browser()
    new_data <- grouped_data()$data()

    data_r$metadata <<- new_data[, meta = T]
    sprintf("Updating data slot with dataframe of %s rows", nrow(new_data))
    zeallot::`%<-%`(c(data, mapping, scale_X_FUN, discrete_y), bin_data(data.table::copy(new_data), do = TRUE))
    data_r$name <<- reactive(dataset_name())
    data_r$extra <<- reactive(list(scale_X_FUN = scale_X_FUN, discrete_y = discrete_y))
    data_r$data <<- reactive(data)
    print(mean(data$asleep))

  }, ignoreInit = TRUE)

  result <- callModule(
    module = esquisse::esquisserServer,
    id = "esquisse",
    data = data_r,
    react = c(dataset_name, apply_filter, last_monitor)
  )



  output$module_out <- renderPrint({
    c(apply_filter())
    str(reactiveValuesToList(result))
  })

  # analyseSleepServer("analyseSleep", grouped_data, dataset_name)

}