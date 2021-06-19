

parse_aesthetics <- function(data, mapping_list, color=NULL, fill=NULL, size=NULL) {
  # the entries in mapping_list will be passed to
  # ggplot transformed into a aesthetic object (call to aes.string)

  other_aesthetics <- list(color=color, fill=fill, size=size)

  for (i in 1:length(other_aesthetics)) {
    if (!is.null(other_aesthetics[[i]])) {
      aes_name <- names(other_aesthetics)[i]
      mapping_list[[aes_name]] <- other_aesthetics[[i]]
    }
  }

  # remove the variables where the value is ""
  # so aes_string does not complain
  aesthetics <- names(mapping_list)
  for (a in aesthetics) {
    if (mapping_list[[a]] == "") mapping_list[[a]] <- NULL
  }


  # make sure the mapping is possible using the provided data
  mapping_list <- lapply(mapping_list,
                         function(col){
                           if(col %in% colnames(data))
                             paste0("`", col, "`")
                           else
                             col
                         })
  return(mapping_list)

}


plotUI <- function(id) {

  ns <- NS(id)

  tagList(
    shiny::tabPanel(
      p("Aesthetics"),
      textInput(ns("color"), label = "Color by", value=NULL),
      textInput(ns("fill"), label = "Fill by", value=NULL),
      textInput(ns("size"), label = "Size by", value=NULL)
    ),
    shiny::tabPanel(
      p("Facet"),
      textInput(ns("facet"), label = "Facet by", value=NULL),
      textInput(ns("facet_row"), label = "Facet row by", value=NULL),
      textInput(ns("facet_col"), label = "Facet col by", value=NULL)
    ),
    wellPanel(
      actionButton(ns("button"), label = "Plot!"),
      plotOutput(ns("plot"))
    )
  )
}

plotServer <- function(id, input_rv, mapping) {

  moduleServer(
    id,
    function(input, output, session) {


      mapping_list_ <- make_labels(mapping)

      mapping <- reactive({
        mapping_list <- parse_aesthetics(input_rv$data, mapping_list_, color=input$color, fill=input$fill, size=input$size)
        mapping <- do.call(ggplot2::aes_string, mapping_list)
        mapping
      })

      p <- eventReactive(input$button, {
        d <- req(input_rv$data)

        if (isTruthy(input$facet) & !isTruthy(input$facet_row) & !isTruthy(input$facet_col)) {

          d$facet_row <- input$facet_row
          d$facet_col <- input$facet_row

          p <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2) +
            facet_grid(facet_row ~ facet_col)
          p
        }

        if (!isTruthy(input$facet) & isTruthy(input$facet_row) & isTruthy(input$facet_col)) {

          d$facet <- input$facet

          gg <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2) +
            facet_wrap("facet")

        } else {

          gg <- ggplot(data = d, mapping()) +
            stat_pop_etho() +
            stat_ld_annotations(color=NA, height=1, alpha=0.2)
        }
        gg

      }, ignoreInit = TRUE)

      output$plot <- renderPlot({
        p()
      })
    }
  )
}