#' Define groups of animals relevant to the user
#'
#' @importFrom shiny NS actionButton tags
defineGroupUI <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    shiny::actionButton(ns('insertBtn'), 'Insert group'),
    shiny::actionButton(ns('removeBtn'), 'Remove group'),
    shiny::div(id = ns("placeholder")),
    shiny::verbatimTextOutput(ns("out"))
  )
}


defineGroupServer <- function(id, scored_data) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      selector <- paste0("#", ns('placeholder'))
      message(sprintf("selector %s", selector))

      groups <- reactiveValues()
      rv <- reactiveValues(data = NULL, name = NULL)

      group_id <- reactive({
        id <- sprintf("defineSingleGroup%d", input$insertBtn)
        id
      })

      observeEvent(input$insertBtn, {
        # message("Inserting UI with id ", my_id)
        shiny::insertUI(
          selector = selector,
          ## wrap element in a div with id for ease of removala
          ui = defineSingleGroupUI(ns(my_id)), immediate = TRUE
        )

        groups[[group_id()]] <- defineSingleGroupServer(group_id(), scored_data)

      }, ignoreInit = TRUE, label = "observe insertBtn")

      observeEvent(input$removeBtn, {
        shiny::removeUI(
          selector = selector
        )
        groups[[input$removeBtn]] <- NULL
      })


      # a reactive that reacts to every apply button
      apply_buttons <- reactive({
        lapply(names(groups), function(x) {
          input[[paste0(x, "-apply")]]
        })
      })

      observeEvent(apply_buttons(), {
        req(scored_data$data)
        rv$data <- group_data(data.table::copy(scored_data$data), groups)
        rv$name <- scored_data$name
      })

      return(rv)

    }
  )
}

defineSingleGroupUI <- function(id) {

  ns <- shiny::NS(id)
  tagList(
    shiny::uiOutput(ns("ui_placeholder")),
    shiny::verbatimTextOutput(ns("scored_data_summary")),
    shiny::verbatimTextOutput(ns("a_filter"))
  )
}

make_filter <- function(data, group_name, variable, operator, value, reverse, metadata) {

  type <- class(data[[variable]])
  message(sprintf('Column %s has type %s\n', variable, type))
  if (!any(type %in% c('numeric', 'integer'))) {
    value <- sprintf("'%s'", value)
  }

  a_filter <- paste(variable, operator, value, sep = " ")
  names(a_filter) <- group_name
  if (reverse) {
    a_filter <- sprintf("! (%s)", a_filter)
  }
  message(sprintf("a_filter = %s", a_filter))
  return(a_filter)
}
#' Process user entered groups of animals
#'
#' Return a character defining an animal group
#' implemented via row filtering in data.tables
#' @import shiny
#' @importFrom fslbehavr rejoin
defineSingleGroupServer <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {

      operators <- c(">", "<", ">=", "<=", "==", "!=", "%in%")

      ns <- session$ns

      rval <- reactiveVal(NULL)

      output$ui_placeholder <- renderUI({
        isolate(
          tagList(
            shiny::textInput(ns("group_name"), label = "Name", value = ""),
            shiny::selectizeInput(ns("variable"), choices = colnames(fslbehavr::rejoin(scored_data$data())), label = "", multiple = FALSE),
            shiny::selectizeInput(ns("operator"), choices = operators, label = "", multiple = FALSE),
            shiny::textInput(ns("value"), label = "", value = ""),
            shiny::checkboxInput(ns("reverse"), label = "Reverse", value = FALSE),
            shiny::checkboxInput(ns("metadata"), label = "Metadata column", value = TRUE),
            shiny::actionButton(ns("apply"), label = "Apply")
          )
        )
      })

      observeEvent(c(input$apply, rv$data[, meta = input$metadata]), {

        new_filter <- make_filter(
          rv$data[, meta = input$metadata], input$group_name, input$variable,
          input$operator, input$value, input$reverse, input$metadata
        )
        rval(new_filter)
      })

      return(rval)
    })
}

edit_data <- function(data, filter, value) {

  group <- NULL

  if (filter != "" & !is.null(value)) {
    metadata <- data[, meta = T]

    names(filter) <- NULL
    metadata[eval(parse(text = filter)), group := value]
    fslbehavr::setmeta(data, metadata)
  }
  return(data)
}

#' Assign groups to flies
#'
#' `group_data` populates the group column of the metadata in a behavr table
#' so plots and downstream analyses can segregate with user provided groups
#'
#' @param data A shiny reactiveValue with a single reactive that yields a behavr table under key input
#' @param groups A shiny reactiveValues object where every reactive returns a named character
#' than can be used to filter rows in the behavr object.
#' @return A reactive that returns the same behavr table but its metadata will have
#' in the column "group", where the filter evals to TRUE, the value given by the name of the filter i.e.
#' the name bound to the character vector in each group.
#' @importFrom data.table `:=`
group_data <- function(data, groups) {

  group <- NULL
  browser()
  if (!"group" %in% colnames(data)) data[, group := "", meta = T]

  if (length(names(groups)) != 0) {
    req(length(names) != 0)
    for (i in 1:length(names(groups))) {
        reactive_val_name <- names(groups)[i]
        group_name <- names(groups[[reactive_val_name]])
        req(group_name)
        a_filter <- groups[[reactive_val_name]]
        grouped_data <- edit_data(data, a_filter, group_name)
    }
  }
  else {
    grouped_data <- data
  }

  return(grouped_data)
}

