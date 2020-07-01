library(shiny)

ui <- function() {
  fluidPage(
    analyseSleepUI("analyseSleep")
  )
}
server <- function(input, output) {

  scored_data <- fslbehavr::toy_activity_data()
  analyseSleepServer("analyseSleep", reactiveValues(data = reactive(scored_data)), reactiveVal("mock"))
}

reactlog::reactlog_enable()
shinyApp(ui = ui(), server = server)

