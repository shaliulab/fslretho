conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug
session_folder <- conf$content$folders$sessions$path

##
# fslretho employs a reactiveValues data structure to transfer data between the appliation's modules,
# which makes it very important to understand how they work and how they are structured

# Structure:

# reactiveValues(data = behavr(), name = character(), time = Sys.time())

# Updates to time are listened to by observeEvents throughout the application,
# which respond and reprocess the contents of data

# FSLretho has a load/save functionality, which makes it necessary to implement functions
# that encapsulate the save / load behavior

save_reactiveValuesRDS <- function(object = rv, ...) {
  d <- reactiveValuesToList(object)
  saveRDS(object = d, ...)
}

read_reactiveValuesRDS <- function(file, ...) {

  rv <- reactiveValues(data = NULL, name = NULL, time = NULL)
  d <- readRDS(file, ...)

  rv$data <- d$data
  rv$name <- d$name
  rv$time <- d$time

  return(rv)
}


get_sessions <- function() {
  session_folder <- file.path(
    FSLRethoConfiguration$new()$content$folders$sessions
  )

  sessions <- list.files(path = session_folder, pattern = "rds")
  sessions <- sapply(sessions, function(x) strsplit(x, split = "\\.rds") %>% unlist %>% .[1])
  names(sessions) <- sessions %>% sapply(., function(x) substr(x, 1, 30))
  sessions <- as.list(sessions)
  if(length(sessions) == 0) {
     sessions <- list("Empty_cache" = "")
  }
  return(sessions)
}


loadSessionUI <- function(id) {

  ns <- NS(id)


  shiny::tagList(
    shiny::tags$li(
      actionButton(ns("button"), "", icon = icon("upload")),
      class = "dropdown user user-menu"
    ),
    shiny::tags$li(
      uiOutput(ns("path_ui")),
      class = "dropdown user user-menu"
    )
  )
}

loadSessionServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        ethoscope = reactiveValues(data = NULL, name = NULL, time = NULL),
        dam = reactiveValues(data = NULL, name = NULL, time = NULL)
      )

      sessions <- reactive({
        input$button
        get_sessions()
      })

      output$path_ui <- renderUI({
        selectizeInput(
          session$ns("path"), label = "", multiple = FALSE,
          selected = sessions()[[1]], choices = sessions()
        )
      })

      observeEvent(input_rv$ethoscope$time, {
        output_rv$ethoscope$data <- input_rv$ethoscope$data
        output_rv$ethoscope$name <- input_rv$ethoscope$name
        output_rv$ethoscope$time <- input_rv$ethoscope$time
      }, ignoreInit = TRUE)

      observeEvent(input_rv$dam$time, {
        output_rv$dam$data <- input_rv$dam$data
        output_rv$dam$name <- input_rv$dam$name
        output_rv$dam$time <- input_rv$dam$time
      }, ignoreInit = TRUE)

      observeEvent(input$button, {
        req(input$path)

        if (DEBUG) message("Loading cached session")
        read_rv <- read_reactiveValuesRDS(file.path(session_folder, paste0(input$path, ".rds")))
        monitor <- ifelse("machine_name" %in% colnames(read_rv$data[,meta=TRUE]) | "xy_dist_log10x1000" %in% colnames(read_rv$data), "ethoscope", "dam")
        output_rv[[monitor]]$data <- read_rv$data
        output_rv[[monitor]]$name <- read_rv$name
        output_rv[[monitor]]$time <- read_rv$time
      }, ignoreInit = TRUE)

      return(output_rv)
    })
}


saveSessionUI <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shiny::tags$li(
      actionButton(ns("button"), "", icon = icon("save")),
      class = "dropdown user user-menu"
    ),

    shiny::tags$li(
      textInput(ns("path"), label = "", value = "", placeholder = "save.rds"),
      class = "dropdown user user-menu"
    )
  )
}

saveSessionServer <- function(id, input_rv) {

  moduleServer(
    id,
    function(input, output, session) {

      filename <- reactive({
        if (tolower(rev(substr(rev(input$path), 1, 3))) == "rds") {
          input$path
        } else {
          paste0(input$path, ".rds")
        }
      })

      observeEvent(input$button, {
        req(input$path)

        # TODO If time is 0 instead of NULL at initialization, I can save
        # all the null checking lines
        if ( !is.null(input_rv$ethoscope$time)) {
          if( !is.null(input_rv$dam$time))
            monitor <- c("ethoscope", "dam")[as.numeric(input_rv$dam$time) > as.numeric(input_rv$ethocope$time)]
          else
            monitor <- "ethoscope"
        } else if (!is.null(input_rv$dam$time)) {
            monitor <- "dam"
        } else {
          if (DEBUG) message("No data is loaded. Nothing is saved")
          return(NULL)
        }

        if (DEBUG) message("Saving session to cache")
        save_reactiveValuesRDS(
          object = input_rv[[monitor]],
          file = file.path(session_folder, filename())
        )
      }, ignoreInit = TRUE)
    }
  )
}


saveLoadSessionUI <- function(id) {
  ns <- NS(id)
  append(
    saveSessionUI(ns("save")),
    loadSessionUI(ns("load"))
  )
}


saveLoadSessionServer <- function(id, input_rv)  {
  moduleServer(
    id,
    function(input, output, session) {
      output_rv <- loadSessionServer("load", input_rv)
      saveSessionServer("save", output_rv)
      return(output_rv)
    }
  )
}