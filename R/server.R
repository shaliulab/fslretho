#' Server function of FSLRetho
#'
#' @import shiny
#' @noRd
server <- function(input, output, session) {

  last_monitor <- shiny::reactiveVal(NULL)
  dataset_name <- shiny::reactiveVal("")
  apply_filter <- shiny::reactiveVal(0)

  output$dataset_name <- shiny::renderText({
    dataset_name()
  })

  message("Point 1")

  raw_data_multiple <- list(
    "ethoscope" = loadEthoscopeServer("loadData-ethoscope", last_monitor, dataset_name),
    "dam" = loadDamServer("loadData-dam", last_monitor, dataset_name)
  )
  message("Point 2")

  # select data slot
  raw_data <- reactive({

    dataset_name()
    last_monitor()

    message(sprintf("Updating raw_data slot with monitor %s, dataset %s", last_monitor(), dataset_name()))
    raw_data_multiple[[last_monitor()]]()
  })


  message("Point 3")

  message("Point 4")

  scored_data <- scoreDataServer("scoreData", raw_data, dataset_name)

  groups <- defineGroupServer("defineGroup", scored_data, apply_filter)

  grouped_data <- set_groups(scored_data, groups, apply_filter)

  viewMetadataServer("viewMetadata", grouped_data)

  analyseSleepServer("analyseSleep", scored_data, dataset_name)

}