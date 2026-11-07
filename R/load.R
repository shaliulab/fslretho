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

#' Load and score DAM data of a linked metadata
#' @param metadata linked metadata (damr::link_dam_metadata)
#' @param params list of scoring parameters: only min_time_immobile applies to DAM data
#' @return behavr table
#' @noRd
load_dam_data <- function(metadata, params) {
  dt <- shiny::withProgress(message = "Loading DAM data", value = 0.5, {
    damr::load_dam(metadata, FUN = sleepr::sleep_dam_annotation, min_time_immobile = params$min_time_immobile)
  })
  if (is.null(dt) || nrow(dt) == 0) stop("none of the flies in the metadata have data")
  behavr::simplify_behavr(dt, meta = TRUE)
}

#' Is a dataset from DAM monitors (as opposed to ethoscopes)?
#' @noRd
is_dam_dataset <- function(data) {
  files <- as.character(unlist(behavr::meta(data)$file_info))
  length(files) > 0 && !any(grepl("\\.db$", files))
}

#' Size of a remote file, from the Content-Length of an HTTP HEAD request (NA if unknown)
#' @noRd
remote_file_size <- function(url) {
  headers <- tryCatch(curlGetHeaders(url), error = function(e) character(0))
  size <- sub("(?i)^content-length:\\s*", "", grep("(?i)^content-length:", headers, value = TRUE, perl = TRUE), perl = TRUE)
  if (length(size) == 0) NA_real_ else as.numeric(trimws(size[length(size)]))
}

#' Link ethoscope metadata to a remote database, downloading the .db files that are needed
#'
#' The remote index (as for scopr::link_ethoscope_metadata_remote) is used to find the .db files.
#' They are copied to result_dir with the same folder structure, and only downloaded when they are
#' missing or their size differs from the remote file. The size is asked to the server rather than
#' read from the index, which is out of date for recordings that are still running.
#' @noRd
link_remote_ethoscope <- function(metadata, remote_dir, index_file, result_dir) {
  if (file.access(result_dir, mode = 2) != 0) {
    stop(sprintf("the local result folder '%s' is not writable, so the .db files cannot be downloaded into it", result_dir))
  }
  old <- options(timeout = 3600)
  on.exit(options(old))

  files <- suppressMessages(scopr:::build_query(result_dir = remote_dir, query = metadata, index_file = index_file))
  relative <- unique(sub(paste0("^", remote_dir, "/?"), "", files$path))
  downloaded <- 0
  shiny::withProgress(message = "Checking the .db files on the remote database", value = 0, {
    for (i in seq_along(relative)) {
      url <- paste(remote_dir, relative[i], sep = "/")
      local <- file.path(result_dir, relative[i])
      remote_size <- remote_file_size(url)
      local_size <- if (file.exists(local)) file.size(local) else NA
      if (is.na(local_size) || is.na(remote_size) || local_size != remote_size) {
        shiny::incProgress(0, detail = sprintf("downloading %s", basename(local)))
        dir.create(dirname(local), recursive = TRUE, showWarnings = FALSE)
        tmp <- paste0(local, ".part")
        utils::download.file(url, tmp, mode = "wb", method = "libcurl", quiet = TRUE)
        if (!file.rename(tmp, local)) stop("could not write ", local)
        downloaded <- downloaded + 1
      }
      shiny::incProgress(1 / length(relative))
    }
  })
  shiny::showNotification(
    sprintf("%d .db file(s) needed: %d downloaded from %s, %d already up to date", length(relative), downloaded, remote_dir, length(relative) - downloaded),
    type = "message", duration = 10
  )
  scopr::link_ethoscope_metadata(metadata, result_dir = result_dir)
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
        radioButtons(ns("source"), label = "Ethoscope data", inline = TRUE,
                     choices = c("On this computer" = "local", "Remote (download what is needed)" = "remote")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'remote'", ns("source")),
          textInput(ns("remote_dir"), label = "Remote ethoscope database (with an index.txt)", value = "")
        ),
        textInput(ns("result_dir"), label = "Ethoscope result folder (on this computer)", value = ""),
        textInput(ns("result_dir_dam"), label = "DAM result folder", value = ""),
        actionButton(ns("submit"), label = "Submit", icon = icon("upload"), class = "btn-primary")
      )
    ),
    column(
      width = 6,
      shinydashboard::box(
        width = NULL, title = "How to load data",
        tags$ol(
          tags$li("Upload a metadata .csv with one row per fly. For ethoscopes, it needs the columns",
                  tags$code("machine_name"), ",", tags$code("date"), ",", tags$code("region_id"), "and",
                  tags$code("reference_hour"), "; for DAM monitors,", tags$code("file"), ",",
                  tags$code("start_datetime"), ",", tags$code("stop_datetime"), "and", tags$code("region_id"), "."),
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
      updateTextInput(session, "result_dir_dam", value = fslretho_conf()$damr$folders$results$path)
      updateTextInput(session, "remote_dir", value = fslretho_conf()$remote_results$path)
      observeEvent(input$go_help, shinydashboard::updateTabItems(parent_session, "tabs", "help"))

      # Data is (re)loaded on submit, on reload and when the scoring parameters change
      observeEvent(c(input$submit, reload(), params()), ignoreInit = TRUE, {
        req(input$submit > 0, input$metadata)
        problems(NULL)

        safely(context = "loading the data", {
          metadata <- read_metadata(input$metadata$datapath)
          monitor <- metadata_monitor(metadata)
          remote <- identical(monitor, "ethoscope") && identical(input$source, "remote")
          index_file <- fslretho_conf()$remote_results$index_file
          result_dir <- if (identical(monitor, "dam")) input$result_dir_dam else input$result_dir
          # a remote database is checked against its index
          found <- if (remote) check_metadata(metadata, input$remote_dir, monitor, index_file)
                   else check_metadata(metadata, result_dir, monitor)
          problems(found)
          if (any(found$level == "error")) {
            show_problems_modal(found, sprintf("The metadata has %d problem(s) to fix before loading", sum(found$level == "error")))
            return()
          }

          if (monitor == "dam") {
            linked <- damr::link_dam_metadata(coerce_metadata(metadata), result_dir = result_dir)
            dt <- load_dam_data(linked, params())
            if (nrow(found) > 0) show_problems_modal(found, "Data loaded, with warnings")
            else showNotification(sprintf("Loaded %d flies", nrow(behavr::meta(dt))), type = "message")
            set_dataset(output_rv, dt, input$metadata$name[1])
            return()
          }

          linked <- if (remote) {
            suppressWarnings(link_remote_ethoscope(coerce_metadata(metadata), input$remote_dir, index_file, result_dir))
          } else {
            suppressWarnings(scopr::link_ethoscope_metadata(coerce_metadata(metadata), result_dir = result_dir))
          }

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

      list(dataset = output_rv, result_dir = reactive(input$result_dir), result_dir_dam = reactive(input$result_dir_dam))
    }
  )
}
