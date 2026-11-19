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
    helpText("Sessions are shared by all users of this server. Ctrl+S saves the session."),
    # Ctrl+S (Cmd+S on Mac) saves the session instead of the web page.
    # If the save dialog is already open, it confirms it
    tags$script(HTML(sprintf(
      "$(document).on('keydown', function(e) {
        if (!(e.ctrlKey || e.metaKey) || e.altKey || (e.key || '').toLowerCase() !== 's') return;
        e.preventDefault();
        var confirm = $('#%s:visible');
        if (confirm.length) { confirm.click(); return; }
        Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
      });",
      ns("shortcut_confirm"), ns("shortcut")
    )))
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

      # name of the last save in this browser session, reused by Ctrl+S.
      # It lives in this Shiny session only, so it is forgotten when the page is
      # reloaded and never shared with other users
      saved_name <- reactiveVal(NULL)

      clean_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", sub("\\.rds$", "", trimws(x)))

      save_session <- function(name) {
        if (is.null(output_rv$data)) {
          showNotification("There is no dataset to save. Load data first.", type = "warning")
          return(invisible(FALSE))
        }
        name <- clean_name(name)
        if (name == "") name <- dataset_basename(output_rv$name)
        safely(context = "saving the session", {
          saveRDS(list(data = output_rv$data, name = output_rv$name), file.path(sessions_folder(), paste0(name, ".rds")))
          saved_name(name)
          updateTextInput(session, "name", value = name)
          showNotification(sprintf("Session saved as %s", name), type = "message")
          refresh(refresh() + 1)
        })
        invisible(TRUE)
      }

      observeEvent(input$save, save_session(input$name))

      # Ctrl+S: save under the name used before, or ask for one the first time
      observeEvent(input$shortcut, {
        if (is.null(output_rv$data)) {
          showNotification("There is no dataset to save. Load data first.", type = "warning")
          return()
        }
        if (!is.null(saved_name())) {
          save_session(saved_name())
          return()
        }
        showModal(modalDialog(
          title = "Save session",
          textInput(session$ns("shortcut_name"), "Session name", value = dataset_basename(output_rv$name), width = "100%"),
          # Enter in the text box saves
          tags$script(HTML(sprintf(
            "setTimeout(function() { $('#%1$s').focus().select().on('keydown', function(e) {
               if (e.key === 'Enter') { e.preventDefault(); $('#%2$s').click(); } }); }, 300);",
            session$ns("shortcut_name"), session$ns("shortcut_confirm")
          ))),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("shortcut_confirm"), "Save", icon = icon("save"), class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      })

      observeEvent(input$shortcut_confirm, {
        removeModal()
        save_session(input$shortcut_name)
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
