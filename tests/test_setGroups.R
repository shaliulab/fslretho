library(shiny)
library(data.table)

ui <- function() {
  fluidPage(
    defineGroupUI("defineGroup")
  )
}
server <- function(input, output) {

  data <- fslbehavr::toy_activity_data()
  scored_data <- reactiveValues(data = reactive(data))
  apply_filter <- shiny::reactiveVal(0)

  groups <- defineGroupServer("defineGroup", scored_data, apply_filter)
  grouped_data <- set_groups(scored_data, groups, apply_filter)

  observe({
    print(head(grouped_data$data()))
  })
}

reactlog::reactlog_enable()
shinyApp(ui = ui(), server = server)
