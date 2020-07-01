library(shiny)

ui <- function() {
  fluidPage(
    defineGroupUI("defineGroup")
  )
}
server <- function(input, output) {

  mybehavr <- behavr_placeholder(mtcars)
  defineGroupServer("defineGroup", reactive(mybehavr))
}

reactlog::reactlog_enable()
shinyApp(ui = ui(), server = server)
