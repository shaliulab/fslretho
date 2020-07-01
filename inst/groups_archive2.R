#' #' Define groups of animals relevant to the user
#' #
#' #' @importFrom shiny NS actionButton tags
#' #
#' # defineGroupUI <- function(id) {
#' #
#' #
#' #   ns <- shiny::NS(id)
#' #   placeholder <- ns('placeholder')
#' #   message(sprintf("placeholder: %s", placeholder))
#' #
#' #   tagList(
#' #     actionButton(ns('insertBtn'), 'Insert group'),
#' #     actionButton(ns('removeBtn'), 'Remove group'),
#' #     uiOutput(ns("placeholder"))
#' #   )
#' # }
#' #
#' # add_group <- function(input, output, session, dt, btn) {
#' #   new_ui <- reactive({
#' #     tagList(
#' #       shiny::textInput(session$ns("group_name"), label = "Name", value = ""),
#' #       shiny::selectizeInput(session$ns("category"), choices = colnames(dt()), label = "", multiple = FALSE),
#' #       shiny::selectizeInput(session$ns("operator"), choices = operators, label = "", multiple = FALSE),
#' #       shiny::textInput(session$ns("value"), label = "", value = ""),
#' #       shiny::checkboxInput(session$ns("reverse"), label = "Reverse", value = FALSE),
#' #       shiny::actionButton(session$ns("apply"), label = "Apply"),
#' #     )
#' #   })
#' #
#' #   shiny::insertUI(selector = selector,
#' #     new_ui()
#' #   })
#' #
#' #   filter <- eventReactive(input$apply, {
#' #
#' #     if (is.null(input$apply)) {
#' #       warning("Apply is NULL")
#' #       NULL
#' #     } else {
#' #       filter <- paste(input$category, input$operator, input$value, sep = " ")
#' #       names(filter) <- input$group_name
#' #
#' #       if (input$reverse) {
#' #         filter <- sprintf("! (%s)", filter)
#' #       }
#' #       filter
#' #     }
#' #   }, ignoreNULL = F)
#' #
#' #   return(filter)
#' #
#' # }
#' #
#' # defineGroupServer <- function(id, dt) {
#' #   moduleServer(
#' #     id,
#' #     function(input, output, session) {
#' #
#' #       insertedUI <- c()
#' #       insertedServer <- reactiveVal(list())
#' #
#' #       observeEvent(input$insertBtn, {
#' #         btn <- input$insertBtn
#' #         add_group(input, output, session, dt, btn)
#' #         selector <- paste0("#", session$ns('placeholder'))
#' #         shiny::insertUI(
#' #           selector = selector,
#' #           ## wrap element in a div with id for ease of removal
#' #           ui =
#' #         )
#' #         insertedUI <<- c(submodule_id, insertedUI)
#' #
#' #         btn <- input$insertBtn
#' #         submodule_id <- sprintf("defineSingleGroup%d", btn)
#' #
#' #         output$conditional_block <- shiny::renderUI({
#' #           operators <- c(">", "<", ">=", "<=", "==", "!=", "%in%")
#' #
#' #           message("Generating conditional input block")
#' #
#' #         })
#' #         insertedServer(c(defineSingleGroupServer(submodule_id, dt)(), insertedServer()))
#' #       })
#' #
#' #       observeEvent(input$removeBtn, {
#' #         shiny::removeUI(selector = paste0('#', insertedUI[length(insertedUI)], "-conditional_block"))
#' #         shiny::removeUI(selector = paste0('#', insertedUI[length(insertedUI)], "-dt_summary"))
#' #
#' #         insertedUI <<- insertedUI[-length(insertedUI)]
#' #         insertedServer(insertedServer()[-length(insertedServer())])
#' #         print("insertedServer")
#' #         print(insertedServer())
#' #       })
#' #
#' #
#' #
#'
#'
#'       return(filter)
#'     }
#'   )
#' }
#'
#'
#' setGroupServer <- function(id, dt, groups) {
#'   moduleServer(
#'     id,
#'     function(input, output, session) {
#'
#'       DT <- reactive({
#'         DT <- dt()
#'         DT[, group := ""]
#'         DT
#'       })
#'
#'       grouped_data <- reactive({
#'
#'         grouped_data <- DT()
#'
#'         for (i in 1:length(groups())) {
#'           filter <- groups()[[i]]
#'           group_name <- names(groups()[[i]])
#'
#'           grouped_data[eval(parse(text = filter)), group := group_name]
#'         }
#'         grouped_data
#'       })
#'       return(grouped_data)
#'     }
#'   )
#' }
