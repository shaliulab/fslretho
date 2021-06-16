get_sessions <- function() {
  cache_dir <- file.path(
    FSLRethoConfiguration$new()$content$scopr$folders$cache$path, "sessions"
  )

  sessions <- list.files(path = cache_dir)#,pattern = "rds")
  names(sessions) <- sessions %>% sapply(., function(x) substr(x, 1, 10))
  sessions <- as.list(sessions)
  sessions <- ifelse(length(sessions) == 0, list("Empty_cache" = ""), sessions)
  return(sessions)
}


loadSessionUI <- function(id) {

  ns <- NS(id)
  sessions <- get_sessions()

  shiny::tagList(
    load_button <- shiny::tags$li(
      actionButton("load", "", icon = icon("upload")),
      class = "dropdown user user-menu"
    ),

    load_ui <- shiny::tags$li(
      selectizeInput(
        "rds_load", label = "", multiple = FALSE,
        selected = sessions[[1]], choices = sessions
      ),
      class = "dropdown user user-menu"
    )
  )
}

saveSessionUI <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    save_button <- shiny::tags$li(
      actionButton("save", "", icon = icon("save")),
      class = "dropdown user user-menu"
    ),

    save_ui <- shiny::tags$li(
      textInput("rds_save", label = "", value = "", placeholder = "save.rds"),
      class = "dropdown user user-menu"
    )
  )
}

saveLoadSessionUI <- function(id) {
  ns <- NS(id)
  append(
    saveSessionUI("save"),
    loadSessionUI("load")
  )
}



builtInOrNewDataServer <- function(id, input_rv) {

  output_rv <- reactiveValues(data=NULL, name=NULL, time=NULL)

  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(input_rv$time, {
        output_rv$data <- input_rv$data
        output_rv$name <- input_rv$name
        output_rv$time <- input_rv$time
      })

      observeEvent(input$load, {
        read_rv <- read_reativeValuesRDS(input$rds_load)
        output_rv$data <- read_rv$data
        output_rv$name <- read_rv$name
        output_rv$time <- read_rv$time
      })

      return(output_rv)
    }
  )
}


saveDataServer <- function(id, input_rv) {

  cache_path <- FSLRethoConfiguration$new()$content$scopr$folders$cache$path

  moduleServer(
    id,
    function(input, output, session) {
      cache_dir <- file.path(cache_path, "sessions")

      observeEvent(input$save, {
        save_reativeValuesRDS(
          object = input_rv$data,
          file = file.path(cache_dir, input$rds_save)
        )
      })
    }
  )
}


saveLoadServer <- function(id, input_rv)  {
  moduleServer(
    id,
    function(input, output, session) {
      loaded_data <- builtInOrNewDataServer("save", input_rv)
      saveDataServer("load", loaded_data)
      return(loaded_data)
    }
  )
}