#' Save and reload whole datasets (sessions) as .rds files in the sessions folder
#'
#' Sessions are shared by all users of the server.
#' @noRd
sessions_folder <- function() fslretho_conf()$folders$sessions$path

list_sessions <- function() {
  sessions <- sub("\\.rds$", "", list.files(sessions_folder(), pattern = "\\.rds$"))
  sessions[order(sessions, decreasing = TRUE)]
}

sessionsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "fsl-sessions",
    tags$p(tags$b("Save the loaded dataset")),
    textInput(ns("name"), label = NULL, placeholder = "session name", width = "100%"),
    actionButton(ns("save"), "Save", icon = icon("save"), class = "btn-block"),
    tags$hr(),
    tags$p(tags$b("Open a saved session")),
    selectizeInput(ns("session"), label = NULL, choices = NULL, width = "100%", options = list(placeholder = "saved sessions")),
    actionButton(ns("load"), "Open", icon = icon("folder-open"), class = "btn-block"),
    helpText("Sessions are shared by all users of this server.")
  )
}

#' @param loaded dataset coming from the Load tab
#' @return dataset: the last one loaded, either from the Load tab or from a saved session
#' @noRd
sessionsServer <- function(id, loaded) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()
      refresh <- reactiveVal(0)

      observeEvent(refresh(), {
        updateSelectizeInput(session, "session", choices = list_sessions())
      })

      observeEvent(loaded$time, {
        set_dataset(output_rv, loaded$data, loaded$name)
      })

      observeEvent(input$save, {
        if (is.null(output_rv$data)) {
          showNotification("There is no dataset to save. Load data first.", type = "warning")
          return()
        }
        name <- gsub("[^A-Za-z0-9_.-]+", "_", sub("\\.rds$", "", input$name))
        if (name == "") name <- dataset_basename(output_rv$name)
        safely(context = "saving the session", {
          saveRDS(list(data = output_rv$data, name = output_rv$name), file.path(sessions_folder(), paste0(name, ".rds")))
          showNotification(sprintf("Session saved as %s", name), type = "message")
          refresh(refresh() + 1)
        })
      })

      observeEvent(input$load, {
        req(input$session)
        safely(context = "loading the session", {
          d <- readRDS(file.path(sessions_folder(), paste0(input$session, ".rds")))
          set_dataset(output_rv, d$data, d$name)
          showNotification(sprintf("Loaded session %s", input$session), type = "message")
        })
      })

      output_rv
    }
  )
}
