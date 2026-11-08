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

  # scopr calls back once per fly (and per interval, if the SD has its own parameters).
  # Flies may be loaded in parallel (ncores, in forked processes), so the count is kept in a file:
  # one byte per fly loaded
  steps <- nrow(metadata) * (1 + length(params$intervals))
  counter <- if (!is.null(progress_file)) paste0(progress_file, ".count")
  if (!is.null(counter)) on.exit(unlink(counter), add = TRUE)
  report_progress(progress_file, "Loading data to memory (R)", detail = sprintf("%d flies, %d core(s)", nrow(metadata), ncores), value = 0)
  callback <- function(detail = NULL) {
    if (is.null(counter)) return(invisible(NULL))
    cat("x", file = counter, append = TRUE)
    report_progress(progress_file, "Loading data to memory (R)", detail = detail, value = min(1, file.size(counter) / steps))
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

#' Experiment start date of each row; the linked metadata has a datetime column instead of date
#' @noRd
fly_date <- function(m) {
  if ("date" %in% names(m)) as.character(m$date) else substr(as.character(m$datetime), 1, 10)
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

# Linking and loading ----
#
# Loading happens in two steps, so users can choose which flies to load:
# 1. link_dataset: check the metadata, find the recording of every fly (downloading it if needed)
#    and list the flies, with the reason why some of them cannot be loaded
# 2. load_linked: load and score the flies selected
# Both run without Shiny, so they can run in a background process (see in_background).

#' Why a fly found by flies_without_data cannot be loaded, and what to do about it
#' @noRd
NO_DATA_FIXES <- c(
  "no tracking data for this ROI in the .db file" = "The tube (ROI) was empty or not tracked, or the recording stopped before the fly was tracked.",
  "the .db file is incomplete (it has no ROI_MAP table)" = "The recording was probably stopped or copied before it was complete."
)

#' Message shown for flies that were linked but could not be scored
#' @noRd
TOO_LITTLE_DATA <- "Too little data to be scored: the fly was tracked for too short a time, or too sparsely."

#' Capitalise the first letter of a sentence
#' @noRd
sentence <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

#' An empty table of flies (see link_dataset)
#' @noRd
no_flies <- function() {
  data.table::data.table(
    id = character(), fsl_row = integer(), machine = character(), region_id = integer(), date = character(),
    from = as.POSIXct(character(), tz = "GMT"), to = as.POSIXct(character(), tz = "GMT"),
    rows = numeric(), fps = numeric(), status = character(), message = character()
  )
}

#' The flies of a linked metadata, with the data available for each
#' @param linked linked metadata, with the column fsl_row (the metadata row of each fly)
#' @return table of flies as in link_dataset
#' @noRd
linked_flies <- function(linked, monitor, progress_file = NULL) {
  if (identical(monitor, "dam")) {
    # DAM files are only read when loading: until then, the period asked for in the metadata
    parse <- function(x) as.POSIXct(substr(as.character(x), 1, 19), tz = "GMT", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d"))
    return(data.table::data.table(
      id = as.character(linked$id), fsl_row = as.integer(linked$fsl_row),
      machine = sapply(linked$file_info, function(x) x$file), region_id = as.integer(linked$region_id),
      date = substr(as.character(linked$start_datetime), 1, 10),
      from = parse(linked$start_datetime), to = parse(linked$stop_datetime),
      rows = NA_real_, fps = NA_real_, status = "ok", message = ""
    ))
  }
  report_progress(progress_file, "Checking the data of each fly", value = 0.9)
  missing <- flies_without_data(linked)
  available <- ethoscope_availability(linked[!as.character(id) %in% missing$id])
  flies <- data.table::data.table(
    id = as.character(linked$id), fsl_row = as.integer(linked$fsl_row), machine = linked$machine_name,
    region_id = as.integer(linked$region_id), date = fly_date(linked), status = "ok", message = ""
  )
  if (nrow(available) > 0) {
    flies <- available[, .(id, from, to, rows = as.numeric(rows), fps)][flies, on = "id"]
  } else {
    flies[, `:=`(from = as.POSIXct(NA, tz = "GMT"), to = as.POSIXct(NA, tz = "GMT"), rows = NA_real_, fps = NA_real_)]
  }
  for (i in seq_len(nrow(missing))) {
    reason <- missing$reason[i]
    fix <- if (reason %in% names(NO_DATA_FIXES)) NO_DATA_FIXES[[reason]] else ""
    flies[id == missing$id[i], `:=`(status = "error", message = trimws(paste0(sentence(reason), ". ", fix)))]
  }
  flies[, names(no_flies()), with = FALSE]
}

#' Check a metadata, find the recording of each fly and list the flies
#'
#' Problems of the whole file (e.g. a missing column) stop everything. Problems of some rows only
#' (e.g. a date with no recording) only exclude those rows: they are listed with the reason,
#' and the other flies can be loaded.
#' @return list with
#' * problems: see check_metadata
#' * monitor: ethoscope or dam
#' * linked: the linked metadata of the flies that can be loaded (NULL if none), with a column fsl_row
#' * flies: one row per fly (or per metadata row that could not be linked), with id, fsl_row (metadata row),
#'   machine, region_id, date, from, to, rows, fps (the data available), status (ok, warning or error)
#'   and message (why the fly cannot be loaded, or a warning)
#' * note: what was downloaded from a remote database
#' * title: why nothing can be loaded, if so
#' @noRd
link_dataset <- function(metadata_paths, source, remote_dir, result_dir, result_dir_dam, progress_file = NULL) {
  report_progress(progress_file, "Checking the metadata", value = 0)
  metadata <- read_metadata(metadata_paths)
  monitor <- metadata_monitor(metadata)
  remote <- identical(monitor, "ethoscope") && identical(source, "remote")
  index_file <- fslretho_conf()$remote_results$index_file
  if (identical(monitor, "dam")) result_dir <- result_dir_dam
  # a remote database is checked against its index
  found <- if (remote) check_metadata(metadata, remote_dir, monitor, index_file)
           else check_metadata(metadata, result_dir, monitor)
  errors <- found[level == "error"]
  if (any(lengths(errors$row_ids) == 0)) {
    return(list(problems = found, monitor = monitor, flies = no_flies(),
                title = sprintf("The metadata has %d problem(s) to fix before linking", nrow(errors))))
  }

  metadata[, fsl_row := seq_len(.N)]
  bad <- sort(unique(unlist(errors$row_ids)))
  # the same fly twice (a warning for ethoscopes): only the first row is used
  key <- intersect(c("machine_name", "date", "time", "region_id"), names(metadata))
  duplicate <- if (identical(monitor, "ethoscope") && all(c("machine_name", "date") %in% key)) {
    which(duplicated(metadata, by = key))
  } else integer(0)

  to_link <- metadata[!fsl_row %in% c(bad, duplicate)]
  linked <- NULL
  note <- NULL
  if (nrow(to_link) > 0) {
    report_progress(progress_file, "Linking the metadata to the recordings", value = 0.1)
    typed <- coerce_metadata(to_link)
    if (monitor == "dam") {
      linked <- damr::link_dam_metadata(typed, result_dir = result_dir)
    } else if (remote) {
      r <- suppressWarnings(link_remote_ethoscope(typed, remote_dir, index_file, result_dir, progress_file))
      linked <- r$linked
      note <- r$note
    } else {
      linked <- suppressWarnings(scopr::link_ethoscope_metadata(typed, result_dir = result_dir))
    }
  }
  flies <- if (!is.null(linked) && nrow(linked) > 0) linked_flies(linked, monitor, progress_file) else no_flies()

  # metadata rows without any fly
  cell <- function(col, rows) if (col %in% names(metadata)) metadata[[col]][rows] else NA
  unlinked <- function(rows, message) {
    if (length(rows) == 0) return(NULL)
    data.table::data.table(
      id = NA_character_, fsl_row = as.integer(rows),
      machine = if (monitor == "dam") cell("file", rows) else cell("machine_name", rows),
      region_id = suppressWarnings(as.integer(cell("region_id", rows))),
      date = if (monitor == "dam") substr(cell("start_datetime", rows), 1, 10) else cell("date", rows),
      from = as.POSIXct(NA, tz = "GMT"), to = as.POSIXct(NA, tz = "GMT"), rows = NA_real_, fps = NA_real_,
      status = "error", message = message
    )
  }
  # every problem of a row, in the order of the metadata check
  row_messages <- function(problems, r) {
    p <- problems[sapply(row_ids, function(ids) r %in% ids)]
    paste(trimws(paste(p$problem, p$fix, sep = ". ")), collapse = " ")
  }
  first_copy <- function(r) {
    same <- metadata[r, key, with = FALSE][metadata, on = key, nomatch = NULL]
    min(same$fsl_row)
  }
  flies <- data.table::rbindlist(c(
    list(flies),
    lapply(bad, function(r) unlinked(r, row_messages(errors, r))),
    lapply(duplicate, function(r) unlinked(r, sprintf("Same fly as row %d: only the first row is used.", first_copy(r) + 1))),
    list(unlinked(setdiff(to_link$fsl_row, flies$fsl_row), "No recording was found for this row."))
  ), use.names = TRUE)

  # warnings about some rows are shown next to their flies, which can still be loaded
  warnings <- found[level == "warning" & lengths(row_ids) > 0]
  for (r in intersect(unlist(warnings$row_ids), flies[status != "error", fsl_row])) {
    flies[fsl_row == r & status != "error", `:=`(status = "warning", message = row_messages(warnings, r))]
  }
  data.table::setorderv(flies, c("fsl_row", "region_id"), na.last = TRUE)

  usable <- if (!is.null(linked)) linked[as.character(id) %in% flies[status != "error", id]]
  if (is.null(usable) || nrow(usable) == 0) {
    return(list(problems = found, monitor = monitor, flies = flies, note = note,
                title = "None of the flies in the metadata can be loaded: the reason is given for each row"))
  }
  list(problems = found, monitor = monitor, linked = usable, flies = flies, note = note)
}

#' Load and score the flies of a linked metadata (see link_dataset)
#' @param linked the linked metadata of the flies to load
#' @return list with data (behavr table), not_loaded (ids of the flies that had too little data to be scored)
#' and, for DAM data, availability (see dam_availability)
#' @noRd
load_linked <- function(linked, monitor, params, progress_file = NULL) {
  linked <- data.table::copy(linked)
  # the metadata row is only needed to show the flies in the Load tab
  if ("fsl_row" %in% names(linked)) linked[, fsl_row := NULL]
  if (identical(monitor, "dam")) {
    report_progress(progress_file, "Loading DAM data", value = 0.5)
    dt <- load_dam_data(linked, params)
    return(list(data = dt, not_loaded = character(0), availability = dam_availability(dt)))
  }
  dt <- load_ethoscope_data(linked, params, progress_file)
  list(data = dt, not_loaded = setdiff(as.character(linked$id), as.character(behavr::meta(dt)$id)))
}

#' The table of flies after loading some of them
#' @param flies table of flies (see link_dataset)
#' @param result result of load_linked, or NULL if nothing was loaded
#' @return the same table, with status loaded for the flies loaded, and an error for those that could not be scored
#' @noRd
flies_after_load <- function(flies, result) {
  flies <- data.table::copy(flies)
  if (is.null(result)) return(flies)
  loaded <- as.character(behavr::meta(result$data)$id)
  flies[id %in% loaded, status := "loaded"]
  flies[id %in% result$not_loaded, `:=`(status = "error", message = TOO_LITTLE_DATA)]
  a <- result$availability
  if (!is.null(a) && nrow(a) > 0) {
    flies[a, on = "id", `:=`(from = i.from, to = i.to, rows = as.numeric(i.rows), fps = i.fps)]
  }
  flies
}

#' Link and load every fly of one or more metadata files (link_dataset, then load_linked)
#' @return list with problems, flies (see link_dataset), data (the scored behavr table, or NULL if
#' nothing could be loaded), note and title (why nothing was loaded)
#' @noRd
load_dataset <- function(metadata_paths, source, remote_dir, result_dir, result_dir_dam, params, progress_file = NULL) {
  l <- link_dataset(metadata_paths, source, remote_dir, result_dir, result_dir_dam, progress_file)
  if (is.null(l$linked)) return(l)
  r <- load_linked(l$linked, l$monitor, params, progress_file)
  list(problems = l$problems, flies = flies_after_load(l$flies, r), data = r$data, note = l$note)
}

# Load tab ----

#' The table of flies, with a checkbox to select each fly that can be loaded
#' @param flies table of flies (see link_dataset and flies_after_load)
#' @param selected ids of the flies selected
#' @param input_id,token the selection is sent by my_javascript.js to the input input_id as list(token, ids)
#' @noRd
flies_table <- function(flies, selected, input_id, token) {
  time <- function(x) if (is.na(x)) "" else format(x, "%Y-%m-%d %H:%M")
  status_cell <- function(f) {
    switch(
      f$status,
      ok = tags$td(class = "fsl-status", tags$span(class = "text-muted", "Ready")),
      loaded = tags$td(class = "fsl-status", tags$span(class = "label label-success", "Loaded"),
                       if (nzchar(f$message)) tags$span(class = "fsl-warning-text", f$message)),
      warning = tags$td(class = "fsl-status fsl-warning-text", icon("exclamation-triangle"), f$message),
      error = tags$td(class = "fsl-status fsl-error-text", icon("times-circle"), f$message)
    )
  }
  tags$table(
    id = input_id, class = "table table-striped table-condensed fsl-flies",
    `data-input` = input_id, `data-token` = token,
    tags$thead(tags$tr(
      tags$th(""), tags$th("Row"), tags$th("Machine"), tags$th("ROI"), tags$th("Fly"), tags$th("From"), tags$th("To"),
      tags$th("Hours"), tags$th("Rows"), tags$th("Frames per second"), tags$th("Status")
    )),
    tags$tbody(lapply(seq_len(nrow(flies)), function(i) {
      f <- flies[i]
      usable <- f$status != "error"
      hours <- as.numeric(difftime(f$to, f$from, units = "hours"))
      tags$tr(
        class = if (!usable) "fsl-fly-error",
        tags$td(if (usable) tags$input(
          type = "checkbox", class = "fsl-fly-select", value = f$id,
          checked = if (f$id %in% selected) NA, `aria-label` = paste("Load", f$id)
        )),
        tags$td(f$fsl_row + 1), tags$td(f$machine), tags$td(if (!is.na(f$region_id)) f$region_id),
        tags$td(if (!is.na(f$id)) tags$small(f$id)),
        tags$td(class = "fsl-nowrap", time(f$from)), tags$td(class = "fsl-nowrap", time(f$to)),
        tags$td(if (!is.na(hours)) format(round(hours, 1), nsmall = 1)),
        tags$td(if (!is.na(f$rows)) format(f$rows, big.mark = ",")),
        tags$td(if (!is.na(f$fps)) signif(f$fps, 3)),
        status_cell(f)
      )
    }))
  )
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
        actionButton(ns("link"), label = "Link", icon = icon("link"), class = "btn-primary"),
        helpText("Link finds the recording of every fly (and downloads it, if Remote) and lists the flies below.")
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
          tags$li("Press Link. The metadata is checked and the flies are listed below, with the data available for each. ",
                  "Flies that cannot be loaded say why."),
          tags$li("Untick the flies you don't want, adjust the scoring parameters in the top bar if needed, ",
                  "and press Load selected flies.")
        ),
        tags$p("See the", actionLink(ns("go_help"), "Help"), "tab for details on every column and parameter.")
      )
    ),
    column(width = 12, uiOutput(ns("report"))),
    column(width = 12, uiOutput(ns("flies_ui")))
  )
}

#' @param reload reactive, the reload button in the header
#' @param params reactive list of scoring parameters
#' @return list with dataset (reactiveValues with data, name, time), result_dir, result_dir_dam and
#' selected_ids (the ids of the flies loaded, if only some of the flies were selected; NULL otherwise)
#' @noRd
loadDataServer <- function(id, reload, params, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      output_rv <- dataset_rv()
      problems <- reactiveVal(NULL)
      # result of link_dataset, with the name of the metadata
      linked <- reactiveVal(NULL)
      # result of load_linked
      loaded <- reactiveVal(NULL)
      selected <- reactiveVal(character(0))
      # the selection sent by the browser is ignored if it belongs to a table of a previous link
      token <- reactiveVal(0)
      busy <- reactiveVal(FALSE)

      updateTextInput(session, "result_dir", value = fslretho_conf()$scopr$folders$results$path)
      updateTextInput(session, "result_dir_dam", value = fslretho_conf()$damr$folders$results$path)
      updateTextInput(session, "remote_dir", value = fslretho_conf()$remote_results$path)
      observeEvent(input$go_help, shinydashboard::updateTabItems(parent_session, "tabs", "help"))

      usable_ids <- function(l) if (is.null(l)) character(0) else l$flies[status != "error", id]

      # a new metadata (or data source) makes the list of flies out of date
      observeEvent(list(input$metadata, input$source), ignoreInit = TRUE, {
        linked(NULL)
        loaded(NULL)
        problems(NULL)
      })

      start <- function() {
        if (busy()) {
          showNotification("Your data is still being linked or loaded; the new request is ignored.", type = "warning")
          return(FALSE)
        }
        busy(TRUE)
        TRUE
      }

      #' @param keep_selection keep the flies that were not selected unselected (reload)
      #' @param then_load load the selected flies once linked
      run_link <- function(keep_selection = FALSE, then_load = FALSE) isolate({
        # evaluated now: then_load usually depends on loaded(), which is reset once linked
        force(keep_selection)
        force(then_load)
        if (is.null(input$metadata)) {
          showNotification("Upload a metadata file first.", type = "warning")
          return()
        }
        if (!start()) return()
        unselected <- if (keep_selection) setdiff(usable_ids(linked()), selected()) else character(0)
        name <- input$metadata$name[1]
        problems(NULL)
        in_background(
          "link_dataset",
          list(
            metadata_paths = input$metadata$datapath, source = input$source, remote_dir = input$remote_dir,
            result_dir = input$result_dir, result_dir_dam = input$result_dir_dam
          ),
          context = "linking the metadata", progress = TRUE,
          on_failed = function() busy(FALSE),
          on_done = function(res) {
            busy(FALSE)
            problems(res$problems)
            if (!is.null(res$note)) showNotification(res$note, type = "message", duration = 10)
            loaded(NULL)
            if (nrow(res$flies) == 0) {
              linked(NULL)
              show_problems_modal(res$problems, res$title)
              return()
            }
            res$name <- name
            token(isolate(token()) + 1)
            selected(setdiff(usable_ids(res), unselected))
            linked(res)
            if (is.null(res$linked)) {
              showNotification(res$title, type = "error", duration = NULL)
              return()
            }
            n_error <- sum(res$flies$status == "error")
            showNotification(
              sprintf("%d flies linked%s", length(usable_ids(res)),
                      if (n_error > 0) sprintf("; %d row(s) cannot be loaded (see the table)", n_error) else ""),
              type = if (n_error > 0) "warning" else "message"
            )
            if (then_load) run_load()
          }
        )
      })

      run_load <- function() isolate({
        l <- linked()
        if (is.null(l) || is.null(l$linked)) {
          showNotification("Press Link first: there are no flies to load.", type = "warning")
          return()
        }
        ids <- intersect(selected(), usable_ids(l))
        if (length(ids) == 0) {
          showNotification("Select at least one fly to load.", type = "warning")
          return()
        }
        if (!start()) return()
        in_background(
          "load_linked",
          list(linked = l$linked[as.character(id) %in% ids], monitor = l$monitor, params = params()),
          context = "loading the data", progress = TRUE,
          on_failed = function() busy(FALSE),
          on_done = function(res) {
            busy(FALSE)
            loaded(res)
            # flies that could not be scored lose their checkbox
            selected(setdiff(isolate(selected()), res$not_loaded))
            n <- nrow(behavr::meta(res$data))
            if (length(res$not_loaded) > 0) {
              showNotification(sprintf("Loaded %d flies; %d had too little data to be scored (see the table)", n, length(res$not_loaded)),
                               type = "warning", duration = NULL)
            } else {
              showNotification(sprintf("Loaded %d flies", n), type = "message")
            }
            set_dataset(output_rv, res$data, l$name)
          }
        )
      })

      observeEvent(input$link, run_link())
      observeEvent(input$load, run_load())
      # reload: link again (downloading recordings that changed) and load the same selection
      observeEvent(reload(), ignoreInit = TRUE, {
        req(input$metadata)
        run_link(keep_selection = TRUE, then_load = !is.null(loaded()))
      })
      # new scoring parameters: score the loaded flies again
      observeEvent(params(), ignoreInit = TRUE, {
        if (!is.null(loaded()) && !is.null(linked())) run_load()
      })

      observeEvent(input$selection, {
        if (identical(as.character(input$selection$token), as.character(token()))) {
          selected(as.character(unlist(input$selection$ids)))
        }
      })

      flies <- reactive({
        l <- req(linked())
        flies_after_load(l$flies, loaded())
      })

      output$flies_ui <- renderUI({
        f <- flies()
        ns <- session$ns
        sel <- isolate(selected())
        all_selected <- all(f[status != "error", id] %in% sel)
        shinydashboard::box(
          width = NULL, title = "Flies", status = "primary", solidHeader = TRUE,
          tags$div(
            class = "fsl-flies-controls",
            actionButton(ns("load"), label = "Load selected flies", icon = icon("play"), class = "btn-primary"),
            tags$button(
              type = "button", id = ns("toggle_all"), class = "btn btn-default fsl-flies-toggle",
              `data-table` = ns("selection"), if (all_selected) "Deselect all" else "Select all"
            ),
            textOutput(ns("n_selected"), inline = TRUE)
          ),
          tags$div(style = "overflow-x: auto;", flies_table(f, sel, ns("selection"), token())),
          helpText("Times are in GMT, as recorded by the ethoscopes / DAM system. Rows are the raw observations",
                   "(ethoscope frames; DAM minutes, known once loaded); frames per second is the number of rows divided",
                   "by the time between the first and the last one. Row is the row of the metadata file, as numbered in Excel.")
        )
      })
      output$n_selected <- renderText({
        sprintf("%d of %d flies selected", length(selected()), sum(flies()$status != "error"))
      })

      output$report <- renderUI({
        p <- req(problems())
        if (nrow(p) == 0) return(NULL)
        shinydashboard::box(width = NULL, title = "Metadata check", status = "warning", solidHeader = TRUE, problems_table(p))
      })

      list(
        dataset = output_rv,
        result_dir = reactive(input$result_dir), result_dir_dam = reactive(input$result_dir_dam),
        selected_ids = reactive({
          l <- loaded()
          if (is.null(l) || is.null(linked())) return(NULL)
          ids <- as.character(behavr::meta(l$data)$id)
          if (setequal(ids, usable_ids(linked()))) NULL else ids
        })
      )
    }
  )
}
