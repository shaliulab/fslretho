

#' Annotate a behavr with a SD treatment progress
#' A new `sd_on` column stating the SD treatment status is added to the passed behavr
#' @param data behavr table
#' @return the same behavr table with a new column `sd_on`.
#' @export
sd_inprogress_annotation <- function(data, time_window_length=10, ...) {

  d <- sleepr::prepare_data_for_motion_detector(data, c("id", "t"), time_window_length=time_window_length, curate=F)
  meta <- behavr::meta(d)
  path <- sapply(meta$file_info, function(x) x$path)
  d_small <- d
  # d_small <- d[!duplicated(t_round), ]
  d_small$t <- NULL
  data.table::setnames(d_small, "t_round", "t")
  # d_small must have keys like the data in a behavr
  data.table::setkey(d_small, id)
  behavr::setmeta(d_small, behavr::meta(data))
  d_small <- parse_sd_daterange(d_small, ...)
  d_small[, sd_on := ((t > xmv(start_sd)) & (t < xmv(end_sd)))]
  return(d_small)
}
attr(sd_inprogress_annotation, "needed_columns") <- function() {
  c()
}
attr(sd_inprogress_annotation, "variables") <- function() {
  c("sd_on")
}
attr(sd_inprogress_annotation, "parameters") <- function() {
  c()
}
attr(sd_inprogress_annotation, "use_meta") <- function() {
  TRUE
}




FUNCTION_MAP <- list(
  `sleep annotation` = list(ethoscope = sleepr::sleep_annotation, dam = sleepr::sleep_dam_annotation),
  `distance annotation` = list(ethoscope = sleepr::sum_movement_detector, dam = NULL),
  `SD in-progress annotation` = list(ethoscope = sd_inprogress_annotation, dam = NULL)
)

conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug


#' @importFrom shiny NS uiOutput
scoreDataUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::sliderInput(ns("velocity_correction_coef"), label = "Threshold (velocity correction coef)", min = 0.001, max = 0.006, value = 0.0048, step = 0.0001),
    shiny::sliderInput(ns("min_time_immobile"), label = "Mimimum time immobile", min = 60, max = 600, value = 300, step = 10),
    actionButton(ns("time300"), label = "Min time immobile = 300"),
    actionButton(ns("time60"), label = "Min time immobile = 60"),
    shiny::sliderInput(ns("time_window_length"), label = "Window duration", min = 5, max = 60, value = 10, step = 5),
    shiny::selectizeInput(
      ns("FUN"), label = "",
      choices = c("sleep annotation", "distance annotation", "SD in-progress annotation"),
      selected = c("sleep annotation", "SD in-progress annotation"),
      multiple=TRUE
    )
  )
  # shiny::uiOutput(ns("scoringInput"))
}

scoreDataServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


      output_rv <- reactiveValues(
        velocity_correction_coef=NULL,
        min_time_immobile=NULL,
        time_window_length=NULL,
        FUN=NULL
      )

      observeEvent(input$time300, {
        updateSliderInput(inputId = "min_time_immobile", value = 300)
      })
      observeEvent(input$time60, {
        updateSliderInput(inputId = "min_time_immobile", value = 60)
      })

      observe({
        output_rv$velocity_correction_coef <- req(input$velocity_correction_coef)
        output_rv$min_time_immobile <- req(input$min_time_immobile)
        output_rv$time_window_length <- req(input$time_window_length)
        output_rv$FUN <- lapply(req(input$FUN), function(f) FUNCTION_MAP[[f]]$ethoscope)
      })


      return(output_rv)
    }
  )
}

#' @importFrom tools toTitleCase
loadDataUI <- function(id, help_text = "") {

  ns <- NS(id)

  tagList(
    mainPanel(
      h2(tools::toTitleCase("Metadata input (DAM/ethoscope)")),
      fluidRow(
        fileInput(inputId = ns("metadata"), label = "",
                         multiple = TRUE,
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"
                         ),
        ),

        textInput(
          inputId = ns("result_dir_ethoscope"), label = "",
          value = FSLRethoConfiguration$new()$content$scopr$folders$results$path
        ),
        textInput(
          inputId = ns("result_dir_dam"), label = "",
          value = FSLRethoConfiguration$new()$content$damr$folders$results$path
        ),

        actionButton(ns("submit"), label = "Submit")
      )
    ),
    sidebarPanel(p(help_text))
  )
}


loadDataServer <- function(id, input_rv, reload) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- reactiveValues(
        ethoscope = reactiveValues(data=NULL, name=NULL, time=NULL),
        dam = reactiveValues(data=NULL, name=NULL, time=NULL)
      )

      submit <- reactive(
        input$submit
      )

      metadata_datapath <- reactiveVal(NULL)

      observeEvent(input$metadata$datapath, {
        metadata_datapath(input$metadata$datapath)
      }, ignoreInit = TRUE)

      observeEvent(input_rv$metadata_datapath, {
        metadata_datapath(input_rv$metadata_datapath)
      }, ignoreInit = TRUE)



      annotation_conf <- scoreDataServer("annotation")

      ethoscope_result <- loadEthoscopeServer("ethoscope", metadata_datapath, submit, reload, input$result_dir_ethoscope,
                                              annotation_conf=annotation_conf)

      observeEvent(ethoscope_result$time, {
        output_rv$ethoscope$data <- ethoscope_result$data
        output_rv$ethoscope$name <- input$metadata[1, "name"]
        output_rv$ethoscope$time <- ethoscope_result$time
      })
      # dam_result <- loadDamServer("dam", metadata_datapath, submit, reload, input$result_dir_dam)

      return(output_rv)
    }
  )
}
