monitorSelectorServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(data = NULL, name = NULL, time = NULL, monitor = NULL)

      for (monitor in c("dam", "ethoscope")) {
        observeEvent(input_rv[[monitor]]$time, {
          output_rv$data <- input_rv[[monitor]]$data
          output_rv$time <- input_rv[[monitor]]$time
          output_rv$name <- input_rv[[monitor]]$name
          output_rv$monitor <- monitor
        }, ignoreInit= TRUE)
      }

    return(output_rv)
    }
  )
}