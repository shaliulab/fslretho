#' Columns kept from the ethoscope data after scoring
#' @noRd
COLUMNS_TO_KEEP <- c(
  "t", "x", "y", "max_velocity", "body_movement", "micromovement", "microbehavior", "interactions",
  "beam_crosses", "moving", "asleep", "is_interpolated"
)

#' Load and score the ethoscope data of a linked metadata
#' @param metadata linked metadata (scopr::link_ethoscope_metadata)
#' @param params list of scoring parameters (see scoringParamsServer)
#' @return behavr table
#' @noRd
load_ethoscope_data <- function(metadata, params) {
  conf <- fslretho_conf()
  ncores <- conf$scopr$ncores
  pb <- get_progress_bar(nrow(metadata), "Loading data to memory (R)", ncores = ncores)
  on.exit(pb$progress$close())

  dt <- do.call(scopr::load_ethoscope, c(
    list(
      metadata = metadata,
      # reference_hour is taken from the metadata, fly by fly
      reference_hour = NA,
      ncores = ncores,
      cache = conf$scopr$folders$cache$path,
      verbose = TRUE,
      callback = pb$update,
      columns_to_keep = COLUMNS_TO_KEEP
    ),
    params
  ))

  if (is.null(dt) || nrow(dt) == 0) stop("none of the flies in the metadata have usable data")

  dt <- integrate_annotations(dt)
  # file_info is a list column; simplify it so the dataset can be saved
  behavr::simplify_behavr(dt, meta = TRUE)
}

#' Human readable description of flies in a linked metadata, e.g. "ETHOSCOPE_008 ROI 1"
#' @noRd
describe_flies <- function(linked) {
  paste(sprintf("%s ROI %s (%s)", linked$machine_name, linked$region_id, fly_date(linked)), collapse = ", ")
}

#' Experiment start date of each row; the linked metadata has a datetime column instead of date
#' @noRd
fly_date <- function(m) {
  if ("date" %in% names(m)) as.character(m$date) else substr(as.character(m$datetime), 1, 10)
}

#' Rows of the (unlinked) metadata that correspond to some flies of the linked metadata
#' @noRd
metadata_rows <- function(metadata, linked) {
  if (!"region_id" %in% names(metadata)) return(integer(0))
  key <- function(m) paste(m$machine_name, fly_date(m), as.integer(m$region_id))
  which(key(metadata) %in% key(linked))
}

loadDataUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      shinydashboard::box(
        width = NULL, title = "Load ethoscope data", status = "primary", solidHeader = TRUE,
        fileInput(
          ns("metadata"), label = "Metadata (.csv)", multiple = TRUE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        textInput(ns("result_dir"), label = "Ethoscope result folder", value = ""),
        actionButton(ns("submit"), label = "Submit", icon = icon("upload"), class = "btn-primary")
      )
    ),
    column(
      width = 6,
      shinydashboard::box(
        width = NULL, title = "How to load data",
        tags$ol(
          tags$li("Upload a metadata .csv with one row per fly and at least the columns",
                  tags$code("machine_name"), ",", tags$code("date"), ",", tags$code("region_id"), "and",
                  tags$code("reference_hour"), "."),
          tags$li("Adjust the scoring parameters in the top bar if needed."),
          tags$li("Press Submit. The metadata is checked first: any problem is listed below, with how to fix it.")
        ),
        tags$p("See the", actionLink(ns("go_help"), "Help"), "tab for details on every column and parameter.")
      )
    ),
    column(width = 12, uiOutput(ns("report")))
  )
}

#' @param reload reactive, the reload button in the header
#' @param params reactive list of scoring parameters
#' @return dataset (reactiveValues with data, name, time)
#' @noRd
loadDataServer <- function(id, reload, params, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()
      problems <- reactiveVal(NULL)

      updateTextInput(session, "result_dir", value = fslretho_conf()$scopr$folders$results$path)
      observeEvent(input$go_help, shinydashboard::updateTabItems(parent_session, "tabs", "help"))

      # Data is (re)loaded on submit, on reload and when the scoring parameters change
      observeEvent(c(input$submit, reload(), params()), ignoreInit = TRUE, {
        req(input$submit > 0, input$metadata)
        problems(NULL)

        safely(context = "loading the data", {
          metadata <- read_metadata(input$metadata$datapath)
          found <- check_metadata(metadata, input$result_dir)
          problems(found)
          if (any(found$level == "error")) {
            show_problems_modal(found, sprintf("The metadata has %d problem(s) to fix before loading", sum(found$level == "error")))
            return()
          }

          linked <- suppressWarnings(scopr::link_ethoscope_metadata(coerce_metadata(metadata), result_dir = input$result_dir))

          missing <- flies_without_data(linked)
          if (nrow(missing) > 0) {
            skipped <- linked[id %in% missing$id]
            found <- rbind(found, new_problem(
              "warning", "region_id", metadata_rows(metadata, skipped),
              sprintf("%d fly(s) have no data and are skipped: %s", nrow(skipped), describe_flies(skipped)),
              paste("Their tube (ROI) was empty or not tracked, or the recording stopped before they were tracked.",
                    "Remove these rows from the metadata to hide this message.")
            ))
            problems(found)
            linked <- linked[!id %in% missing$id]
          }
          if (nrow(linked) == 0) {
            show_problems_modal(found, "None of the flies in the metadata have data")
            return()
          }

          dt <- load_ethoscope_data(linked, params())

          not_loaded <- linked[!as.character(id) %in% as.character(behavr::meta(dt)$id)]
          if (nrow(not_loaded) > 0) {
            found <- rbind(found, new_problem(
              "warning", "region_id", metadata_rows(metadata, not_loaded),
              sprintf("%d fly(s) were skipped because they have too little data: %s", nrow(not_loaded), describe_flies(not_loaded)),
              "These flies were tracked for too short a time (or too sparsely) to be scored."
            ))
            problems(found)
          }
          if (nrow(found) > 0) show_problems_modal(found, "Data loaded, with warnings")
          else showNotification(sprintf("Loaded %d flies", nrow(behavr::meta(dt))), type = "message")

          set_dataset(output_rv, dt, input$metadata$name[1])
        })
      })

      output$report <- renderUI({
        p <- req(problems())
        if (nrow(p) == 0) return(NULL)
        shinydashboard::box(width = NULL, title = "Metadata check", status = "warning", solidHeader = TRUE, problems_table(p))
      })

      list(dataset = output_rv, result_dir = reactive(input$result_dir))
    }
  )
}
