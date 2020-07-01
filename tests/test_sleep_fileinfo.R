library(shiny)

ui <- function() {
  fluidPage(
    analyseSleepUI("analyseSleep")
  )
}
server <- function(input, output) {

  file_info <- NULL

  scored_data <- fslbehavr::toy_activity_data()
  metadata <- scored_data[, meta = T]
  metadata[ , file_info := list(list(path = "/ethoscope_data/results/sqlite3.db", file = "sqlite3.db"))]

  fslbehavr::setmeta(scored_data, metadata)

  analyseSleepServer("analyseSleep", reactive(scored_data), reactiveVal("mock"))
}

reactlog::reactlog_enable()
shinyApp(ui = ui(), server = server)
