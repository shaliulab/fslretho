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
load_ethoscope_data <- function(metadata, params, progress_file = NULL) {
  conf <- fslretho_conf()
  ncores <- conf$scopr$ncores

  # scopr calls back once per fly (and per interval, if the SD has its own parameters)
  steps <- nrow(metadata) * (1 + length(params$intervals))
  done <- 0
  report_progress(progress_file, "Loading data to memory (R)", value = 0)
  callback <- function(detail = NULL) {
    done <<- done + 1
    report_progress(progress_file, "Loading data to memory (R)", detail = detail, value = min(1, done / steps))
  }

  dt <- do.call(scopr::load_ethoscope, c(
    list(
      metadata = metadata,
      # reference_hour is taken from the metadata, fly by fly
      reference_hour = NA,
      ncores = ncores,
      cache = conf$scopr$folders$cache$path,
      verbose = TRUE,
      callback = callback,
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
  dt <- damr::load_dam(metadata, FUN = sleepr::sleep_dam_annotation, min_time_immobile = params$min_time_immobile)
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
#' @return list with linked (the linked metadata) and note (what was downloaded)
#' @noRd
link_remote_ethoscope <- function(metadata, remote_dir, index_file, result_dir, progress_file = NULL) {
  if (file.access(result_dir, mode = 2) != 0) {
    stop(sprintf("the local result folder '%s' is not writable, so the .db files cannot be downloaded into it", result_dir))
  }
  old <- options(timeout = 3600)
  on.exit(options(old))

  files <- suppressMessages(scopr:::build_query(result_dir = remote_dir, query = metadata, index_file = index_file))
  relative <- unique(sub(paste0("^", remote_dir, "/?"), "", files$path))
  downloaded <- 0
  for (i in seq_along(relative)) {
    url <- paste(remote_dir, relative[i], sep = "/")
    local <- file.path(result_dir, relative[i])
    remote_size <- remote_file_size(url)
    local_size <- if (file.exists(local)) file.size(local) else NA
    if (is.na(local_size) || is.na(remote_size) || local_size != remote_size) {
      report_progress(
        progress_file, sprintf("Downloading %s (%d of %d)", basename(local), i, length(relative)),
        detail = if (is.na(remote_size)) NULL else sprintf("%.0f MB from %s", remote_size / 1e6, remote_dir),
        value = (i - 1) / length(relative)
      )
      dir.create(dirname(local), recursive = TRUE, showWarnings = FALSE)
      # unique temporary name: two users may download the same file at the same time
      tmp <- tempfile(pattern = basename(local), tmpdir = dirname(local), fileext = ".part")
      on.exit(unlink(tmp), add = TRUE)
      utils::download.file(url, tmp, mode = "wb", method = "libcurl", quiet = TRUE)
      if (!file.rename(tmp, local)) stop("could not write ", local)
      downloaded <- downloaded + 1
    }
  }
  list(
    linked = scopr::link_ethoscope_metadata(metadata, result_dir = result_dir),
    note = sprintf("%d .db file(s) needed: %d downloaded from %s, %d already up to date",
                   length(relative), downloaded, remote_dir, length(relative) - downloaded)
  )
}

#' When the raw data of each ethoscope fly starts and ends, how many rows (frames) it has and its frame rate
#' @param linked linked metadata of the loaded flies
#' @return data.table with id, machine, region_id, from, to (POSIXct, GMT), rows and fps
#' @noRd
ethoscope_availability <- function(linked) {
  paths <- sapply(linked$file_info, function(x) x$path)
  data.table::rbindlist(lapply(unique(paths), function(path) {
    selected <- paths == path
    rows <- linked[selected]
    start <- as.numeric(suppressMessages(scopr::get_metadata(path))$date_time)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
    on.exit(RSQLite::dbDisconnect(con))
    tables <- RSQLite::dbListTables(con)
    data.table::rbindlist(lapply(seq_len(nrow(rows)), function(i) {
      table <- paste0("ROI_", rows$region_id[i])
      if (!table %in% tables) return(NULL)
      q <- RSQLite::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n, MIN(t) AS t0, MAX(t) AS t1 FROM %s", table))
      seconds <- (q$t1 - q$t0) / 1000
      data.table::data.table(
        id = as.character(rows$id[i]), machine = rows$machine_name[i], region_id = rows$region_id[i],
        from = as.POSIXct(start + q$t0 / 1000, origin = "1970-01-01", tz = "GMT"),
        to = as.POSIXct(start + q$t1 / 1000, origin = "1970-01-01", tz = "GMT"),
        rows = q$n, fps = if (isTRUE(seconds > 0)) q$n / seconds else NA_real_
      )
    }))
  }))
}

#' Same as ethoscope_availability, for DAM data: one row per minute
#' @noRd
dam_availability <- function(dt) {
  meta <- data.table::as.data.table(behavr::meta(dt))
  d <- data.table::as.data.table(dt)[, .(t0 = min(t), t1 = max(t), rows = .N), by = id]
  d <- meta[, .(id, file = basename(as.character(unlist(file_info))), region_id, start = as.POSIXct(start_datetime, tz = "GMT"))][d, on = "id"]
  d[, .(id = as.character(id), machine = file, region_id, from = start + t0, to = start + t1, rows, fps = 1 / 60)]
}

#' Read, check, link, load and score the data described by one or more metadata files
#'
#' Runs without Shiny, so it can run in a background process (see in_background).
#' @return list with problems (see check_metadata), data (the scored behavr table, or NULL if
#' nothing could be loaded), availability (see ethoscope_availability), title (why nothing was loaded)
#' and note (remote downloads)
#' @noRd
load_dataset <- function(metadata_paths, source, remote_dir, result_dir, result_dir_dam, params, progress_file = NULL) {
  report_progress(progress_file, "Checking the metadata", value = 0)
  metadata <- read_metadata(metadata_paths)
  monitor <- metadata_monitor(metadata)
  remote <- identical(monitor, "ethoscope") && identical(source, "remote")
  index_file <- fslretho_conf()$remote_results$index_file
  if (identical(monitor, "dam")) result_dir <- result_dir_dam
  # a remote database is checked against its index
  found <- if (remote) check_metadata(metadata, remote_dir, monitor, index_file)
           else check_metadata(metadata, result_dir, monitor)
  if (any(found$level == "error")) {
    return(list(problems = found, title = sprintf("The metadata has %d problem(s) to fix before loading", sum(found$level == "error"))))
  }

  if (monitor == "dam") {
    linked <- damr::link_dam_metadata(coerce_metadata(metadata), result_dir = result_dir)
    report_progress(progress_file, "Loading DAM data", value = 0.5)
    dt <- load_dam_data(linked, params)
    return(list(problems = found, data = dt, availability = dam_availability(dt)))
  }

  note <- NULL
  if (remote) {
    r <- suppressWarnings(link_remote_ethoscope(coerce_metadata(metadata), remote_dir, index_file, result_dir, progress_file))
    linked <- r$linked
    note <- r$note
  } else {
    linked <- suppressWarnings(scopr::link_ethoscope_metadata(coerce_metadata(metadata), result_dir = result_dir))
  }

  missing <- flies_without_data(linked)
  fixes <- c(
    "no tracking data for this ROI in the .db file" = "Their tube (ROI) was empty or not tracked, or the recording stopped before they were tracked.",
    "the .db file is incomplete (it has no ROI_MAP table)" = "The recording was probably stopped or copied before it was complete."
  )
  for (reason in unique(missing$reason)) {
    skipped <- linked[id %in% missing[reason == missing$reason, id]]
    found <- rbind(found, new_problem(
      "warning", "region_id", metadata_rows(metadata, skipped),
      sprintf("%d fly(s) are skipped because %s: %s", nrow(skipped), reason, describe_flies(skipped)),
      paste(if (reason %in% names(fixes)) fixes[[reason]] else "", "Remove these rows from the metadata to hide this message.")
    ))
  }
  linked <- linked[!id %in% missing$id]
  if (nrow(linked) == 0) return(list(problems = found, note = note, title = "None of the flies in the metadata have data"))

  dt <- load_ethoscope_data(linked, params, progress_file)

  not_loaded <- linked[!as.character(id) %in% as.character(behavr::meta(dt)$id)]
  if (nrow(not_loaded) > 0) {
    found <- rbind(found, new_problem(
      "warning", "region_id", metadata_rows(metadata, not_loaded),
      sprintf("%d fly(s) were skipped because they have too little data: %s", nrow(not_loaded), describe_flies(not_loaded)),
      "These flies were tracked for too short a time (or too sparsely) to be scored."
    ))
  }
  report_progress(progress_file, "Counting the data of each fly", value = 1)
  availability <- ethoscope_availability(linked[as.character(id) %in% as.character(behavr::meta(dt)$id)])
  list(problems = found, data = dt, availability = availability, note = note)
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
    column(width = 12, uiOutput(ns("report"))),
    column(width = 12, uiOutput(ns("availability_ui")))
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
      availability <- reactiveVal(NULL)

      updateTextInput(session, "result_dir", value = fslretho_conf()$scopr$folders$results$path)
      updateTextInput(session, "result_dir_dam", value = fslretho_conf()$damr$folders$results$path)
      updateTextInput(session, "remote_dir", value = fslretho_conf()$remote_results$path)
      observeEvent(input$go_help, shinydashboard::updateTabItems(parent_session, "tabs", "help"))

      loading <- reactiveVal(FALSE)

      # Data is (re)loaded on submit, on reload and when the scoring parameters change
      observeEvent(c(input$submit, reload(), params()), ignoreInit = TRUE, {
        req(input$submit > 0, input$metadata)
        if (loading()) {
          showNotification("Your data is still loading; the new request is ignored.", type = "warning")
          return()
        }
        problems(NULL)
        loading(TRUE)
        name <- input$metadata$name[1]

        task_finished <- function() loading(FALSE)
        in_background(
          "load_dataset",
          list(
            metadata_paths = input$metadata$datapath, source = input$source, remote_dir = input$remote_dir,
            result_dir = input$result_dir, result_dir_dam = input$result_dir_dam, params = params()
          ),
          context = "loading the data", progress = TRUE,
          on_failed = task_finished,
          on_done = function(res) {
            loading(FALSE)
            problems(res$problems)
            availability(res$availability)
            if (!is.null(res$note)) showNotification(res$note, type = "message", duration = 10)
            if (is.null(res$data)) {
              show_problems_modal(res$problems, res$title)
              return()
            }
            if (nrow(res$problems) > 0) show_problems_modal(res$problems, "Data loaded, with warnings")
            else showNotification(sprintf("Loaded %d flies", nrow(behavr::meta(res$data))), type = "message")
            set_dataset(output_rv, res$data, name)
          }
        )
      })

      output$availability_ui <- renderUI({
        req(availability())
        shinydashboard::box(
          width = NULL, title = "Data available per fly", status = "primary", solidHeader = TRUE,
          tableOutput(session$ns("availability_table")),
          helpText("Times are in GMT, as recorded by the ethoscopes / DAM system. Rows are the raw observations",
                   "(ethoscope frames; DAM minutes); frames per second is the number of rows divided by the time between the first and the last one.")
        )
      })
      output$availability_table <- renderTable({
        a <- data.table::copy(req(availability()))
        a[, .(
          Fly = id, Machine = machine, ROI = region_id,
          From = format(from, "%Y-%m-%d %H:%M"), To = format(to, "%Y-%m-%d %H:%M"),
          Hours = format(round(as.numeric(difftime(to, from, units = "hours")), 1), nsmall = 1),
          Rows = format(rows, big.mark = ","), `Frames per second` = signif(fps, 3)
        )]
      }, striped = TRUE, spacing = "s")

      output$report <- renderUI({
        p <- req(problems())
        if (nrow(p) == 0) return(NULL)
        shinydashboard::box(width = NULL, title = "Metadata check", status = "warning", solidHeader = TRUE, problems_table(p))
      })

      list(dataset = output_rv, result_dir = reactive(input$result_dir), result_dir_dam = reactive(input$result_dir_dam))
    }
  )
}
