#' Server function of FSLRetho
#'
#' @import shiny
#' @noRd
server <- function(input, output, session) {

  last_monitor <- shiny::reactiveVal(NULL)
  dataset_name <- shiny::reactiveVal("")

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
  raw_data <- eventReactive(last_monitor(), {
    message(sprintf("Updating raw_data slot with monitor %s", last_monitor()))
    raw_data_multiple[[last_monitor()]]()
  }, ignoreNULL = T)

  message("Point 3")

  observeEvent(dataset_name(), {
    print(raw_data())
  }, ignoreInit=TRUE, ignoreNULL=TRUE)

  message("Point 4")

  scored_data <- scoreDataServer("scoreData", raw_data, dataset_name)

  analyseSleepServer("analyseSleep", scored_data, dataset_name)

}