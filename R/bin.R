#' Bin time series of boolean variables of high frequency (>= 1/min)
#' so they become a numerical time series of a lower frequency
#' The meaning of the new time series depends of the binning function used
#' i.e. if mean is used, the time series is the fraction of time spent in the
#' TRUE state in each window
#' Relevant for moving, asleep and interactions variables

FUN_choices <- c("mean", "median", "max", "min", "P_doze", "P_wake")
PARETO <- TRUE
functions <- list(mean, median, max, min, sleepr::p_doze, sleepr::p_wake)
names(functions) <- FUN_choices
Y_VARS <- c(
  "asleep", "moving", "interactions", "is_interpolated", "x",
  "beam_crosses", "max_velocity", "interval", "sd_on", "duration"
)


conf <- FSLRethoConfiguration$new()
DEBUG <- TRUE

binDataUI <- function(id, binning_variable="asleep") {

  ns <- NS(id)
  tagList(
    sliderInput(ns("summary_time_window"), label = "Summary time window",
                value = 30, min = 5, max = 120, step = 5),
    selectizeInput(ns("summary_FUN"), label = "Summary function", choices = FUN_choices, selected = "mean")
    # textInput(ns("y"), label = "Y axis", value=binning_variable),
    # selectizeInput(inputId = ns("y"), label = "Y axis", choices = binning_variable,
    # multiple=TRUE,
    # selected = binning_variable
    # ),
    # tags$h2("Quality refinement"),
    # checkboxInput(ns("pareto"), label = "Apply pareto principle", value = FALSE),
    # checkboxInput(ns("pareto_sd"), label = "If pareto is applied, should it be during SD only?", value = TRUE)

    # uiOutput(ns("y_ui"))
  )
}


#' @param y column to bin over, overrides whatever the user may pass in the input
#' @param summary_time_window width of the bins, in minutes, overrides whatever the user may pass in the input
#' @import behavr
#' @import shiny
#' @importFrom data.table copy
binDataServer <- function(id, input_rv, y = NULL, summary_time_window = NULL, summary_FUN = NULL, preproc_FUN=NULL, allow_pareto=FALSE, ...) {

  output_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)


  moduleServer(
    id,
    function(input, output, session) {

      metadata <- reactive({
        behavr::meta(input_rv$data)
      })

      summary_time_window_r <- reactive({
        if (is.null(summary_time_window)) {
          input$summary_time_window
        } else {
          summary_time_window
        }
      })


      summary_FUN_r <- reactive({
        if (is.null(summary_FUN)) {
          input$summary_FUN
        } else {
          summary_FUN
        }
      })


      preproc_data <- reactive({

        if (!"interval" %in% colnames(input_rv$data)) {
          input_rv$data$interval <- "default"
        }

        if (is.null(preproc_FUN)) {
          # just use the data as is
          input_rv$data
        } else {
          # preprocess it
          lapply(unique(input_rv$data$interval), function(interv) {
            dt <- preproc_FUN(data=input_rv$data, ...)
            dt$interval <- interv
            dt
          }) %>% Reduce(function(x, y) {
            dt <- rbind_behavr(x, y);
            behavr::setmeta(dt, metadata())
            dt
          }, .)
        }
      })

      variables <- reactive({

        req(preproc_data())

        x <- input_rv$variables
        if (!is.null(preproc_FUN)) {
          x <- c(attr(preproc_FUN, "variables")(), x)
        }
        x <- x[x %in% colnames(preproc_data())]
        x

      })

      dt <- reactive({

        req(input_rv$data)
        input_data <- preproc_data()
        if (DEBUG) message(paste0("Binning data using ", summary_FUN_r()))

        # if ("interval" %in% colnames(input_data)) {
        interval_values <- unique(input_data$interval)
        # } else {
        #   interval_values <- "default"
        #   input_data$interval <- "default"
        # }


        y_vars <- Y_VARS[Y_VARS %in% colnames(input_data)]

        dts <- lapply(y_vars, function(y_var) {


          if(y_var %in% c("interval", "sd_on")) {
            fun <- function(x) names(table(x))[1]
          } else if (y_var %in% "x") {
            fun <- pareto_sd
          } else {
            fun <- functions[[summary_FUN_r()]]
          }

          dt2 <- lapply(interval_values, function(interv) {
            data  <- input_data[interval == interv, ]
            dt <- behavr::bin_all(
              data = data,
              y = y_var,
              x = "t",
              x_bin_length = behavr::mins(summary_time_window_r()),
              FUN = fun
            )
            dt$interval <- interv
            setkey(dt, id, interval)
            dt
          })

          dt <- Reduce(rbind_behavr, dt2)
          setkey(dt, id)
          behavr::setmeta(dt, metadata())
          setkey(dt, id, interval)
        })


        dts_by_id <- lapply(dts[[1]][, as.character(unique(id))], function(id_val) {

          dt <- Reduce(function(x, y) {
            print(colnames(x))
            merge_behavr(x[id == id_val, ], y[id == id_val, ], merge_meta=F)
          }, dts)
          setkey(dt, id)
          behavr::setmeta(dt, metadata())
          dt
        })

        dt <- Reduce(rbind_behavr, dts_by_id)# %>% rejoin
        # I need to do set meta again because rbin_behavr is replicating every metadata row
        # i.e. it is rbinding identical metadatas
        behavr::setmeta(dt, metadata())
        # tryCatch(
        #   behavr::rejoin(dt),
        #   error = function(e) {
        #     browser()
        #   }
        # )
        dt
      })


      observeEvent(c(input_rv$time, summary_FUN_r(), summary_time_window_r()), {
        output_rv$data <- dt()
        output_rv$name <- input_rv$name
        output_rv$time <- Sys.time()
      }, ignoreInit = FALSE)

      return(output_rv)
    }
  )}

#' A version of sleepr::bout_analysis that allows programmatic input
#' @seealso sleepr::bout_analysis
#' @inheritParams sleepr::bout_analysis
#' @export
bout_analysis <- function(data, ...) {
  sleepr::bout_analysis_standard(data = data, ...)
}
attr(bout_analysis, "variables") <- function() {"duration"}