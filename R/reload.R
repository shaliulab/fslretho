reloadUI <- function(id) {
  tags$li(
    actionButton("button", "", icon = icon("redo")),
    class = "dropdown user user-menu"
    )
}



reloadModuleServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        # req(!is.null(input$reloadData))
        input$button
      })
    }
  )
}
