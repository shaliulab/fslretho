choices <- c("mean", "median", "max", "min",
             "P_doze", "P_wake")

functions <- c(mean, median, max, min, fslsleepr::p_doze, fslsleepr::p_wake)
names(functions) <- choices

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
  refresh_plot <- reactiveVal(0)
  observeEvent({
    dataset_name()
    apply_filter()
    last_monitor()
    scored_data$data()
    input$summary_time_window
    input$summary_FUN
  }, {
    # input$grouping
    # browser()
    refresh_plot(refresh_plot() + 1)
  }, ignoreNULL = F, ignoreInit = TRUE)

  observeEvent(input$browser, {
    browser()
  })


  grouped_data <- reactive({
    set_groups(scored_data, groups, refresh_plot)
  })


  viewMetadataServer("viewMetadata", grouped_data)

  sleep_rv <- reactiveValues(
    data = reactive(fslbehavr::toy_activity_data()),
    metadata = reactive(NULL),
    name = reactive("data"),
    extra = reactiveValues(
        scale_X_FUN = reactive(NULL),
        discrete_y = reactive(FALSE),
        summary_FUN = reactive(mean),
        summary_time_window = reactive(30 * 60)
      )
  )

  bout_rv <- reactiveValues(
    data = reactive(fslbehavr::toy_activity_data()),
    metadata = reactive(NULL),
    name = reactive("data"),
    extra = reactiveValues(
      scale_X_FUN = reactive(NULL),
      discrete_y = reactive(FALSE),
      summary_FUN = reactive(mean),
      summary_time_window = reactive(30 * 60)
    )
  )


  bout_data <- reactive({
    # browser()
    fslsleepr::bout_analysis(asleep, grouped_data()$data())
  })


  new_data <- reactiveVal(NULL)


  observeEvent(refresh_plot(), {
    sprintf("Updating dataset")
    if (dataset_name() != "NONE") {
      new_data(grouped_data()$data())
      sleep_rv$metadata <<- new_data()[, meta = T]
      bout_rv$metadata <<- new_data()[, meta = T]
      sleep_rv$name <<- reactive(dataset_name())
      bout_rv$name <<- reactive(dataset_name())
      sleep_rv$data <<- reactive(data.table::copy(new_data()))
      bout_rv$data <<- reactive(data.table::copy(bout_data()))
      summary_FUN <- ifelse(is.null(input$summary_FUN), "mean", input$summary_FUN)
      sleep_rv$extra$summary_FUN <<- reactive(functions[[summary_FUN]])
      bout_rv$extra$summary_FUN <<- reactive(functions[[summary_FUN]])
      summary_time_window <- ifelse(is.null(input$summary_time_window), 30, input$summary_time_window)
      sleep_rv$extra$summary_time_window <<- reactive(summary_time_window * 60)
      bout_rv$extra$summary_time_window <<- reactive(summary_time_window * 60)
    }
  }, ignoreInit = FALSE)

  # need(isolate(refresh_plot() != 0), label = "")

  analyse_sleep <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseSleep",
    data = sleep_rv,
    refresh_plot = refresh_plot,
    debug = FSLRethoConfiguration$new()$content[["debug"]],
    raw_data = new_data
  )

  analyse_bout <- callModule(
    module = esquisse::esquisserServer,
    id = "analyseBout",
    data = bout_rv,
    refresh_plot = refresh_plot,
    debug = FSLRethoConfiguration$new()$content[["debug"]],
    raw_data = new_data
  )

  output$analyseSleep_out <- renderPrint({
    c(apply_filter())
    str(reactiveValuesToList(analyse_sleep))
  })

  output$analyseBout_out <- renderPrint({
    c(apply_filter())
    str(reactiveValuesToList(analyse_bout))
  })

  # analyseSleepServer("analyseSleep", grouped_data, dataset_name)

}