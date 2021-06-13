TYPES <- list(
  "character" = character,
  "numeric" = numeric,
  "integer" = integer,
  "factor" = factor,
  "logical" = logical
)

#' Add  fields available in a table to another table missing  them
#' @param x Receiving table
#' @param xx Donating table
extend_datatable <- function(x, xx) {
  new_cols <- setdiff(colnames(xx), colnames(x))
  new_cols_class <- xx[, new_cols, with = F] %>% as.list %>% lapply(., class) %>% unlist

  if(!is.null(new_cols_class)) {
    for (i in 1:length(new_cols_class)) {
      type <- new_cols_class[i]
      if (!type %in% names(TYPES)) {
        stop(paste0("Passed unknown type ", type))
      }
      x[, names(new_cols_class)[i]] <- TYPES[[type]]()
    }
  }
  return(x)
}

#' Populate a pair of placeholder data.tables with the loaded data
#'
#' Populate a dependency generating pair of placeholders
#' with the data loaded by the user.
#' The placeholders contain the standard ethoscope and dam fields
#' and are extended with more if the provided data carries them
extend_dataset <- function(dt, metadata, new_dataset, monitor) {

   local_meta <- new_dataset[[monitor]]$data[, meta = T]
   local_meta$source <- monitor

   if (monitor == "ethoscope") {
     local_meta$datetime <- as.character(local_meta$datetime)
     local_meta$experiment_start <- local_meta$datetime
   } else {
     local_meta$start_datetime <- as.character(local_meta$start_datetime)
     local_meta$stop_datetime <- as.character(local_meta$stop_datetime)
     local_meta$experiment_start <- local_meta$start_datetime
   }
  local_dt <- as.data.table(new_dataset[[monitor]]$data)

  metadata <- extend_datatable(metadata, local_meta)
  local_meta <- extend_datatable(local_meta, metadata)
  metadata <- rbind(metadata, local_meta[, colnames(metadata), with = F])

  dt <- extend_datatable(dt, local_dt)
  local_dt <- extend_datatable(local_dt, dt)
  dt <- rbind(dt, local_dt[, colnames(dt), with = F])

  setkey(metadata, id)
  setkey(dt, id)
  return(list(
    metadata = metadata,
    dt = dt
  ))
}

#' Initialize a sensible placeholder with the right fields
#' but no data
behavr_placeholder <- function() {

  # placeholder metadata
  metadata <- data.table::data.table(id = factor(),
                         file_info = character(),
                         machine_name = character(),
                         machine_id = character(),
                         datetime = character(),
                         region_id = integer(),
                         reference_hour = numeric(),
                         source = character()
  )

  # placeholder table
  dt <- data.table::data.table(
    id = factor(),
    t = numeric(),
    x = numeric(),
    y = numeric(),
    max_velocity = numeric(),
    interactions = integer(),
    beam_crosses = integer(),
    moving = logical(),
    asleep = logical(),
    is_interpolated = logical()
  )

  # create a placeholder behavr table using the placeholder
  data.table::setkey(metadata, id)
  data.table::setkey(dt, id)
  behavr::setmeta(dt, metadata)
  return(dt)
}

#' Choose either the ethoscope or the DAM dataset
#'
#' @param data A reactiveValues with an ethoscope and a dam slot
#' @return A reactiveValues with slots data name and time of the selected dataset
unifyDatasetsServer <- function(id="", data) {


  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      unified_data <- reactive({


        placeholder <- behavr_placeholder()
        dt <- placeholder

        # push the loaded data to the placeholders
        if (!is.null(data$ethoscope$data)) {
          # push the loaded ethoscope data
          extension <- extend_dataset(dt, metadata, data, "ethoscope")
        }

        if (!is.null(data$dam$data)) {
          # push the loaded dam data
          extension <- extend_dataset(dt, metadata, data, "dam")
        } else {
          warning("TODO: Shall I run?")
        }

        dt <- extension$dt
        metadata <- extension$metadata
        setmeta(dt, metadata)

        if (nrow(dt) == 0) {
          return(NULL)
        } else {
          return(dt)
        }
      })

      dataset_name <- reactive(
        "Dataset"
      )

      dataset_time <- reactive({
        c(data$ethoscope$time, data$dam$time)[1]
      })

      observe({
        rv$data <- unified_data()
        rv$name <- dataset_name()
        rv$time <- dataset_time()
      })
      return(rv)
    }
  )
}
