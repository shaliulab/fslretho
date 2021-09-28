

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

minimal_micromovement_detector <- function(data,
                                           mm_window_length,
                                           mm_threshold = 20,
                                           masking_duration = 6,
                                           curate=TRUE
                                           ) {

  d <- sleepr::prepare_data_for_motion_detector(data,
                                   c("t", "body_movement"),
                                   mm_window_length,
                                   "has_interacted",
                                   curate=curate)

  ## Masking
  ## Intended to ignore activity within masking_duration (default 6) seconds
  ## after an interaction happens
  ## ----
  if(!"has_interacted" %in% colnames(d)){
    if(masking_duration > 0)
      warning("Data does not contain an `has_interacted` column.
              Cannot apply masking!.
              Set `masking_duration = 0` to ignore masking")
    d[, has_interacted := 0]
  }

  # create a unique identifier for every interaction
  # this is done so we can split d
  # so rows within the same block share last interaction
  d[,interaction_id := cumsum(has_interacted)]

  # masked becomes TRUE if t is within masking_duration
  # after t[1]
  # since the first row of the block
  # represents the last interaction
  # the time of the interaction is t[1]
  d[,
    masked := t < (t[1] + masking_duration),
    by=interaction_id
  ]

  d[ , body_movement := ifelse(masked & interaction_id != 0, 0, body_movement)]

  # remove the interaction_id and masked columns
  # to preserve state
  d[,interaction_id := NULL]
  d[,masked := NULL]


  d_small <- d[, .(
    max_movement = max(body_movement[min(2, .N):.N]),
    interactions = as.integer(sum(has_interacted))
  ), by = "t_round"]

  d_small[, micromovement := max_movement > mm_threshold]

  data.table::setnames(d_small, "t_round", "t")
  return(d_small)
}

microbehavior_annotation <- function(
  data,
  mm_window_length = 10, #s
  mm_time_immobile = 1, #s
  motion_detector_FUN = minimal_micromovement_detector,
  columns_to_keep = c("t", "max_movement", "interactions","microbehavior", "micromovement",
                      "beam_crosses", "is_interpolated"),
  ...
){

  wrapped <- function(d){
    if(nrow(d) < 100) {
      warning("Dataset is very small")
      return(NULL)
    }


    # todo if t not unique, stop
    d_small <- motion_detector_FUN(d, mm_window_length,...)

    if(key(d_small) != "t")
      stop("Key in output of motion_classifier_FUN MUST be `t'")

    if(nrow(d_small) < 1)
      return(NULL)
    # the times to  be queried
    time_map <- data.table::data.table(t = seq(from=d_small[1,t], to=d_small[.N,t], by=mm_window_length),
                                       key = "t")
    missing_val <- time_map[!d_small]

    d_small <- d_small[time_map,roll=T]
    d_small[,is_interpolated := FALSE]
    d_small[missing_val,is_interpolated:=TRUE]
    d_small[is_interpolated == T, max_movement := 0]
    d_small[is_interpolated == T, micromovement := F]
    d_small[, microbehavior := sleepr:::sleep_contiguous(!micromovement,
                                                         fs=1/mm_window_length,
                                                         min_valid_time=mm_time_immobile)]

    d_small <- stats::na.omit(d[d_small,
                                on=c("t"),
                                roll=T])
    d_small[, intersect(columns_to_keep, colnames(d_small)), with=FALSE]
  }

  # browser()

  if(is.null(key(data)))
    return(wrapped(data))
  data[,
       wrapped(.SD),
       by=key(data)]

}


attr(microbehavior_annotation, "needed_columns") <- function(motion_detector_FUN = minimal_micromovement_detector,
                                                     ...){
  needed_columns <- attr(motion_detector_FUN, "needed_columns")
  if(!is.null(needed_columns))
    needed_columns(...)
}

attr(microbehavior_annotation, "variables") <- function() {
  c("microbehavior", "micromovement")
}

attr(microbehavior_annotation, "updater") <- function(args) {
  rlang::fn_fmls(microbehavior_annotation)$time_window_length <- args$time_window_length
  rlang::fn_fmls(microbehavior_annotation)$min_time_immobile <- args$min_time_immobile

  # update velocity correction coef
  motion_detector_FUN <- get(as.character(rlang::fn_fmls(microbehavior_annotation)$motion_detector_FUN))
  rlang::fn_fmls(motion_detector_FUN)$velocity_correction_coef <- args$velocity_correction_coef
  rlang::fn_fmls(microbehavior_annotation)$motion_detector_FUN <- motion_detector_FUN
  return(microbehavior_annotation)
}

attr(microbehavior_annotation, "parameters") <- function(motion_detector_FUN = minimal_micromovement_detector, ...) {

  # arguments of microbehavior_annotation
  args <- names(formals(microbehavior_annotation))
  args <- unique(c(args, names(formals(minimal_micromovement_detector))))
  args <- args[args != "..."]
  args <- args[args != "data"]
  args <- args[args != "motion_detector_FUN"]
  return(args)
}

integrate_annotations <- function(data) {

  if (all(c("micromovement", "asleep") %in% colnames(data))) {
    data[, micromovement := micromovement & asleep]
  }
  if (all(c("microbehavior", "asleep") %in% colnames(data))) {
    data[, microbehavior := microbehavior & asleep]
  }
  return(data)
}

FUNCTION_MAP <- list(
  `sleep annotation` = list(ethoscope = sleepr::sleep_annotation, dam = sleepr::sleep_dam_annotation),
  `distance annotation` = list(ethoscope = sleepr::sum_movement_detector, dam = NULL),
  `SD in-progress annotation` = list(ethoscope = sd_inprogress_annotation, dam = NULL),
  `Microbehavior annotation` = list(ethoscope = microbehavior_annotation, dam = NULL)
)

conf <- FSLRethoConfiguration$new()
DEBUG <- conf$content$debug


#' @importFrom shiny NS uiOutput
scoreDataUI <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(fluidRow(
      column(3, shiny::sliderInput(ns("velocity_correction_coef"), label = "Threshold (velocity correction coef)", min = 0.001, max = 0.006, value = 0.0048, step = 0.0001)),
      column(3, shiny::sliderInput(ns("min_time_immobile"), label = "Mimimum time immobile", min = 60, max = 600, value = 300, step = 10)),
      column(3, tagList(actionButton(ns("time300"), label = "Min time immobile = 300"), actionButton(ns("time60"), label = "Min time immobile = 60")))
    ),
  fluidRow(
      column(3, shiny::sliderInput(ns("time_window_length"), label = "Window duration", min = 5, max = 60, value = 10, step = 5)),
      column(4, shiny::selectizeInput(
          ns("FUN"), label = "",
          choices = names(FUNCTION_MAP),
          selected = c("sleep annotation", "SD in-progress annotation"),
          multiple=TRUE
        ),
      column(2, shiny::checkboxInput(ns("curate"), label = "Curate?", value = TRUE))
      )
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
        FUN=NULL,
        curate=NULL
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
        output_rv$curate <- input$curate
        output_rv$FUN <- lapply(req(input$FUN), function(f) FUNCTION_MAP[[f]]$ethoscope)
      })


      return(output_rv)
    }
  )
}


superScoreDataServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      default_conf <- scoreDataServer("default")
      sd_conf <- scoreDataServer("sd")
      output_rv = reactiveValues()

      observeEvent(c(lapply(default_conf, invisible), lapply(sd_conf, invisible)), {

        for (arg_name in names(default_conf)) {
          output_rv[[arg_name]] <- default_conf[[arg_name]]
        }
        for (arg_name in names(sd_conf)) {
          output_rv[[paste0(arg_name, "_sd")]] <- sd_conf[[arg_name]]
        }
      }, ignoreInit = FALSE)


      observeEvent(input$differentiate_SD, {
        if (! input$differentiate_SD) output_rv$intervals <- NULL
        else output_rv$intervals <- list(interval_sd = "SD")
      })

      return(output_rv)
  })
}

superScoreDataUI <- function(id) {

  ns <- NS(id)

  inputPanel(
    div(
      fluidRow(
        tags$h2("Default annotation parameters"),
        scoreDataUI(ns("default"))
      ), style="width: 600px; max-width: 600px"
    ) %>% htmltools::tagAppendAttributes(., style='width: 600px; max-width: 600px'),
    div(
      fluidRow(
        tags$h2("SD annotation parameters"),
        checkboxInput(ns("differentiate_SD"), label = "Differentiate SD", value = FALSE),
        scoreDataUI(ns("sd"))
      ), style="width: 600px; max-width: 600px"
    )  %>% htmltools::tagAppendAttributes(., style='width: 600px; max-width: 600px')
  ) #%>% htmltools::tagAppendAttributes(., style='width: 600px; max-width: 600px')
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


loadDataServer <- function(id, reload) {
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

      metadata_datapath <- reactive({
        input$metadata$datapath
      })


      annotation_conf <- superScoreDataServer("annotation")


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
