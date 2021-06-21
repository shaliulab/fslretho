monitorSelectorServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(data = NULL, name = NULL, time = NULL, monitor = NULL)

      for (monitor in c("dam", "ethoscope")) {
        observeEvent(input_rv[[monitor]]$time, {
          for (field in names(input_rv[[monitor]])) {
              output_rv[[field]] <- input_rv[[monitor]][[field]]
          }
          output_rv$monitor <- monitor
        }, ignoreInit= TRUE)
      }

    return(output_rv)
    }
  )
}