TYPES <- list(
  "character" = character,
  "numeric" = numeric,
  "integer" = integer,
  "factor" = factor,
  "logical" = logical
)
extend_datatable <- function(metadata, extra_meta) {
  new_cols <- setdiff(colnames(extra_meta), colnames(metadata))
  # new_cols_class <- extra_meta[, new_cols, with = F] %>% apply(., 2, class)
  new_cols_class <- extra_meta[, new_cols, with = F] %>% as.list %>% lapply(., class) %>% unlist

  if(!is.null(new_cols_class)) {
    for (i in 1:length(new_cols_class)) {
      type <- new_cols_class[i]
      if (!type %in% names(TYPES)) {
        stop(glue::glue("Passed unknown type {type}"))
      }
      metadata[, names(new_cols_class)[i]] <- TYPES[[type]]()
    }
  }

  metadata
}

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

unify_datasets <- function(id="", data) {

  # observe({
  #   browser()
  #   data$ethoscope$data
  # })

  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(
        data = NULL,
        name = NULL,
        time = NULL
      )

      unified_data <- reactive({

        metadata <- data.table(id = factor(),
                               file_info = character(),
                               machine_name = character(),
                               machine_id = character(),
                               datetime = character(),
                               region_id = integer(),
                               reference_hour = numeric(),
                               source = character()
        )

        dt <- data.table(
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
        setkey(metadata, id)
        setkey(dt, id)
        setmeta(dt, metadata)

        if (!is.null(data$ethoscope$data)) {

          extension <- extend_dataset(dt, metadata, data, "ethoscope")
          dt <- extension$dt
          metadata <- extension$metadata
        }

        if (!is.null(data$dam$data)) {

          extension <- extend_dataset(dt, metadata, data, "dam")
          dt <- extension$dt
          metadata <- extension$metadata

        }
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
