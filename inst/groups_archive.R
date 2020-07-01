#' Define groups of animals relevant to the user
#'
#' @importFrom shiny NS actionButton tags
defineGroupUI <- function(id) {


  ns <- shiny::NS(id)
  placeholder <- ns('placeholder')
  message(sprintf("placeholder: %s", placeholder))

  tagList(
    actionButton(ns('insertBtn'), 'Insert group'),
    actionButton(ns('removeBtn'), 'Remove group'),
    div(id = ns("placeholder"))
  )
}

defineGroupServer <- function(id, dt) {
  moduleServer(
    id,
    function(input, output, session) {

      insertedUI <- c()
      insertedServer <- reactiveVal(list())

      observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        submodule_id <- sprintf("defineSingleGroup%d", btn)
        message(sprintf("submodule_id: %s", submodule_id))
        message("Inserting UI")
        selector <- paste0("#", session$ns('placeholder'))

        message(sprintf("selector %s", selector))

        shiny::insertUI(
          selector = selector,
          ## wrap element in a div with id for ease of removal
          ui = defineSingleGroupUI(session$ns(submodule_id))
        )
        insertedUI <<- c(submodule_id, insertedUI)
      }, priority = 10)

      observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        submodule_id <- sprintf("defineSingleGroup%d", btn)

        output[[paste0(submodule_id, "-conditional_block")]] <- shiny::renderUI({
          operators <- c(">", "<", ">=", "<=", "==", "!=", "%in%")

          message("Generating conditional input block")
          tagList(
            shiny::textInput(session$ns("group_name"), label = "Name", value = ""),
            shiny::selectizeInput(session$ns(sprintf("%s-category", submodule_id)), choices = colnames(dt()), label = "", multiple = FALSE),
            shiny::selectizeInput(session$ns("operator"), choices = operators, label = "", multiple = FALSE),
            shiny::textInput(session$ns("value"), label = "", value = ""),
            shiny::checkboxInput(session$ns("reverse"), label = "Reverse", value = FALSE),
            shiny::actionButton(session$ns(sprintf("%s-category", submodule_id)), label = "Apply1"),
            shiny::actionButton(session$ns("apply"), label = "Apply"),
            shiny::actionButton(paste0("defineSingleGroup1-apply"), label = "Apply"),
            shiny::actionButton("apply", label = "Apply")
          )
        })
        insertedServer(c(defineSingleGroupServer(submodule_id, dt)(), insertedServer()))


        print("insertedServer")
        print(insertedServer())
      }, priority = 10)


      observeEvent(input$removeBtn, {
        shiny::removeUI(selector = paste0('#', insertedUI[length(insertedUI)], "-conditional_block"))
        shiny::removeUI(selector = paste0('#', insertedUI[length(insertedUI)], "-dt_summary"))

        insertedUI <<- insertedUI[-length(insertedUI)]
        insertedServer(insertedServer()[-length(insertedServer())])
        print("insertedServer")
        print(insertedServer())
      })


      observeEvent(insertedServer(), {
        browser()
      })

      return(insertedServer)
    }
  )
}

defineSingleGroupUI <- function(id) {

  ns <- shiny::NS(id)
  message(sprintf("Conditional block full id: %s", ns("conditional_block")))
  tagList(
    shiny::uiOutput(ns("conditional_block")),
    shiny::verbatimTextOutput(ns("dt_summary"))
    # shiny::verbatimTextOutput(ns("metadata_summary"))
  )

}

#' Process user entered groups of animals
#'
#' Return a character defining an animal group
#' implemented via row filtering in data.tables
#' @import shiny
defineSingleGroupServer <- function(id, dt) {
  moduleServer(
    id,
    function(input, output, session) {

      message(sprintf("id of defineSingleGroupServer: %s", id))



      output$dt_summary <- shiny::renderPrint({
        message("Computing dt_summary")
        cat(
          summary(
            dt()
          )
        )
      })

      # output$metadata_summary <- shiny::renderPrint({
      #   message("Computing metadata_summary")
      #   if ("behavr" %in% class(dt())) {
      #     cat(
      #       summary(
      #         dt()[, meta = TRUE]
      #       )
      #     )
      #   } else {
      #     cat("")
      #   }
      # })

      filter <- eventReactive(input$apply, {

        if (is.null(input$apply)) {
          warning("Apply is NULL")
          NULL
        } else {
          filter <- paste(input$category, input$operator, input$value, sep = " ")
          names(filter) <- input$group_name


          if (input$reverse) {
            filter <- sprintf("! (%s)", filter)
          }
          filter
        }
      }, ignoreNULL = F)

      return(filter)
    }
  )
}


setGroupServer <- function(id, dt, groups) {
  moduleServer(
    id,
    function(input, output, session) {

      DT <- reactive({
        DT <- dt()
        DT[, group := ""]
        DT
      })

      grouped_data <- reactive({

        grouped_data <- DT()

        for (i in 1:length(groups())) {
          filter <- groups()[[i]]
          group_name <- names(groups()[[i]])

          grouped_data[eval(parse(text = filter)), group := group_name]
        }
        grouped_data
      })
      return(grouped_data)
    }
  )
}

