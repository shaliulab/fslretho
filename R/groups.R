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


defineGroupServer <- function(id, scored_data, apply_filter) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      selector <- paste0("#", ns('placeholder'))
      message(sprintf("selector %s", selector))
      groups <- reactiveValues()


      new_id <- reactive({
        new_id <- sprintf("defineSingleGroup%d", input$insertBtn)
        message(sprintf("new_id: %s", new_id))
        new_id
      })



      observeEvent(input$insertBtn, {

        my_id <- new_id()
        message("Inserting UI with id ", my_id)
        shiny::insertUI(
          selector = selector,
          ## wrap element in a div with id for ease of removala
          ui = defineSingleGroupUI(ns(my_id)), immediate = TRUE
        )

        message("Pushing new reactive to groups")
        #browser
        groups[[isolate(my_id)]] <<- defineSingleGroupServer(new_id(), scored_data, apply_filter)$a_filter
        # TODO Line does not work and line above does. Why?
        # groups[[new_id()]] <<- reactive(a_filter())

      }, ignoreInit = TRUE, label = "observe insertBtn")


      output$out <- renderPrint({
        lapply(names(groups), function(x) {
          y <- groups[[x]]()
          y
        })
      })
      return(groups)
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
defineSingleGroupServer <- function(id, scored_data, apply_filter) {
  moduleServer(
    id,
    function(input, output, session) {

      operators <- c(">", "<", ">=", "<=", "==", "!=", "%in%")

      ns <- session$ns

      output$scored_data_summary <- shiny::renderPrint({
        message("Computing scored_data_summary")
        # print(head(scored_data$data()))
        cat(
          summary(
            scored_data$data()
          )
        )
      })

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


      DT <- reactive({
        scored_data$data()[, meta = input$metadata]
      })

      a_filter <- eventReactive(input$apply, {

        apply_filter(apply_filter() + 1)

        make_filter(
          DT(), input$group_name, input$variable,
          input$operator, input$value, input$reverse, input$metadata
        )
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      my_filter <- reactiveValues(a_filter = reactive(c("NULL" = "")))
      isolate({
        my_filter$a_filter <- a_filter
      })


      output$a_filter <- renderPrint({
        a_filter()
      })

      return(my_filter)
    }
  )
}

edit_data <- function(data, filter, value) {

  group <- NULL
  #browser

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
  #browser
  lapply(shiny::reactiveValuesToList(groups), function(x) x())

  if (!"group" %in% colnames(data)) data[, group := "", meta = T]

  if (length(names(groups)) != 0) {
    for (i in 1:length(names(groups))) {
      reactive_val_name <- names(groups)[i]
      group_name <- names(groups[[reactive_val_name]]())
      req(group_name)
      a_filter <- groups[[reactive_val_name]]()
      grouped_data <- edit_data(data, a_filter, group_name)
    }
  }
  else {
    grouped_data <- data
  }

  return(grouped_data)
}

#' @importFrom data.table copy
set_groups <- function(scored_data, groups, btn) {

  grouped_data <- eventReactive(btn(), {
    print("reactive")
    group_data(data.table::copy(scored_data$data()), groups)
  }, label = "Set group to name in UI")

  return(list(data = grouped_data))
}

