#' Columns every metadata file needs, by monitor
#' @noRd
REQUIRED_COLUMNS <- list(
  ethoscope = c("machine_name", "date", "reference_hour"),
  dam = c("file", "start_datetime", "stop_datetime")
)

#' Columns that are kept as text when typing the metadata
#' @noRd
TEXT_COLUMNS <- c("machine_name", "date", "time", "file", "start_datetime", "stop_datetime")

#' Which monitor (ethoscope or DAM) a metadata file describes, from its columns
#' @return "ethoscope", "dam", or NA if it cannot be told
#' @noRd
metadata_monitor <- function(metadata) {
  cols <- names(metadata)
  ethoscope <- any(c("machine_name", "date") %in% cols)
  dam <- any(c("file", "start_datetime", "stop_datetime") %in% cols)
  if (ethoscope && !dam) return("ethoscope")
  if (dam && !ethoscope) return("dam")
  NA_character_
}

#' Read one or more metadata .csv files into R, without any type conversion
#'
#' All columns are read as text so that problems (e.g. a date that Excel turned
#' into a number) can be reported exactly as the user wrote them.
#' Lines starting with # are ignored. The separator (, or ;) is detected automatically.
#' @param metadata_path character, paths to metadata .csv files
#' @return data.table with character columns
#' @export
read_metadata <- function(metadata_path) {
  read_one <- function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    # Byte order mark written by Excel's "CSV UTF-8" format
    lines[1] <- sub("^﻿", "", lines[1])
    lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(gsub("[,;]", "", lines)))]
    if (length(lines) < 2) stop(sprintf("The metadata file %s has no rows below the header", basename(path)))
    dt <- data.table::fread(
      text = lines, colClasses = "character", header = TRUE,
      na.strings = c("", "NA", "NaN"), strip.white = TRUE
    )
    data.table::setnames(dt, trimws(names(dt)))
    dt
  }
  data.table::rbindlist(lapply(metadata_path, read_one), use.names = TRUE, fill = TRUE)
}

#' Convert the columns of a metadata read with read_metadata to their natural type
#' (e.g. region_id to integer). Dates, times and names (TEXT_COLUMNS) stay as text.
#' @noRd
coerce_metadata <- function(metadata) {
  metadata <- data.table::copy(metadata)
  for (col in setdiff(names(metadata), TEXT_COLUMNS)) {
    data.table::set(metadata, j = col, value = utils::type.convert(metadata[[col]], as.is = TRUE))
  }
  metadata
}

#' Read and type a metadata file, ready for scopr::link_ethoscope_metadata or damr::link_dam_metadata
#' @param metadata_path Absolute path to one or more metadata.csv files
#' @export
load_metadata <- function(metadata_path) {
  coerce_metadata(read_metadata(metadata_path))
}

# Reporting ----

#' Rows of the metadata as the user sees them in a spreadsheet (row 1 is the header)
#' @noRd
format_rows <- function(rows) {
  rows <- sort(unique(rows)) + 1
  if (length(rows) == 0) return("")
  if (length(rows) > 10) return(paste0(paste(rows[1:10], collapse = ", "), ", ... (", length(rows), " rows)"))
  paste(rows, collapse = ", ")
}

new_problem <- function(level, column, rows, problem, fix) {
  data.table::data.table(level = level, column = column, rows = format_rows(rows), problem = problem, fix = fix)
}

#' Collect problems for each distinct offending value of a column
#' @param values character vector, one value per metadata row
#' @param describe function taking one value and returning NULL (value is fine)
#' or a list(problem, fix)
#' @noRd
problems_by_value <- function(values, column, describe, level = "error") {
  rows <- which(!is.na(values))
  issues <- lapply(unique(values[rows]), function(v) {
    d <- describe(v)
    if (is.null(d)) return(NULL)
    new_problem(level, column, which(values == v), d$problem, d$fix)
  })
  data.table::rbindlist(issues)
}

# Dates ----

is_iso_date <- function(x) {
  grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) & !is.na(as.Date(x, format = "%Y-%m-%d", optional = TRUE))
}

#' Guess what date a spreadsheet date serial number stands for
#'
#' Excel and LibreOffice store dates as the number of days since 1899-12-30;
#' other programs count days since 1970-01-01. Return the interpretation that falls
#' in a plausible range for an experiment.
#' @noRd
guess_serial_date <- function(n) {
  candidates <- c(as.Date(n, origin = "1899-12-30"), as.Date(n, origin = "1970-01-01"))
  plausible <- candidates[candidates >= as.Date("2010-01-01") & candidates <= Sys.Date() + 365]
  if (length(plausible) == 0) return(NA)
  plausible[1]
}

EXCEL_DATE_FIX <- paste(
  "Type the date as YYYY-MM-DD (e.g. 2021-06-18).",
  "In Excel or LibreOffice, first format the date column as Text (right click > Format cells > Text),",
  "otherwise the program converts it again when you save the .csv."
)

#' Explain why a date is not in YYYY-MM-DD format
#' @return NULL if the date is fine, list(problem, fix) otherwise
#' @noRd
describe_date <- function(x) {
  if (is_iso_date(x)) return(NULL)

  # 20210618
  if (grepl("^[0-9]{8}$", x) && !is.na(d <- as.Date(x, format = "%Y%m%d", optional = TRUE))) {
    return(list(
      problem = sprintf("Date '%s' has no dashes", x),
      fix = sprintf("Write it as %s.", d)
    ))
  }

  # 44365, the number of days since 1899-12-30 (Excel) or 1970-01-01
  if (grepl("^[0-9]+([.,]0+)?$", x)) {
    guess <- guess_serial_date(as.numeric(sub(",", ".", x)))
    return(list(
      problem = paste0(
        sprintf("Date '%s' is a number, not a date. ", x),
        "Spreadsheet programs (Excel, LibreOffice) store dates as a count of days, ",
        "and sometimes save that count in the .csv instead of the date",
        if (!is.na(guess)) sprintf(": %s most likely means %s.", x, guess) else "."
      ),
      fix = EXCEL_DATE_FIX
    ))
  }

  # 18/06/2021, 6/18/2021, 18.06.21 ... (regional formats used by Excel)
  m <- regmatches(x, regexec("^([0-9]{1,2})[/.-]([0-9]{1,2})[/.-]([0-9]{2}|[0-9]{4})$", x))[[1]]
  if (length(m) == 4) {
    a <- as.integer(m[2]); b <- as.integer(m[3]); y <- as.integer(m[4])
    if (y < 100) y <- 2000 + y
    candidates <- unique(c(
      if (a <= 31 && b <= 12) sprintf("%04d-%02d-%02d", y, b, a),  # day first
      if (b <= 31 && a <= 12) sprintf("%04d-%02d-%02d", y, a, b)   # month first
    ))
    candidates <- candidates[is_iso_date(candidates)]
    return(list(
      problem = paste0(
        sprintf("Date '%s' is in a regional format, probably changed by Excel. ", x),
        if (length(candidates) == 1) sprintf("It most likely means %s.", candidates)
        else if (length(candidates) > 1) sprintf("It could mean %s.", paste(candidates, collapse = " or "))
        else ""
      ),
      fix = EXCEL_DATE_FIX
    ))
  }

  # 2021/06/18, 2021.06.18, 2021-6-18
  m <- regmatches(x, regexec("^([0-9]{4})[/.-]([0-9]{1,2})[/.-]([0-9]{1,2})$", x))[[1]]
  if (length(m) == 4) {
    d <- sprintf("%s-%02d-%02d", m[2], as.integer(m[3]), as.integer(m[4]))
    if (is_iso_date(d)) return(list(problem = sprintf("Date '%s' is not in YYYY-MM-DD format", x), fix = sprintf("Write it as %s.", d)))
  }

  # 2021-06-18 00:00:00
  d <- substr(x, 1, 10)
  if (nchar(x) > 10 && is_iso_date(d)) {
    return(list(problem = sprintf("Date '%s' includes a time", x), fix = sprintf("Write only the date: %s. Use a separate 'time' column for the time.", d)))
  }

  list(
    problem = sprintf("'%s' is not a valid date", x),
    fix = "Dates must be in YYYY-MM-DD format, e.g. 2021-06-18, and exist in the calendar."
  )
}

is_iso_datetime <- function(x) {
  grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", x) &
    !is.na(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}

#' Explain why a date-time is not in YYYY-MM-DD HH:MM:SS format
#' @param time_required if FALSE, a date alone (YYYY-MM-DD) is also fine
#' @return NULL if the value is fine, list(problem, fix) otherwise
#' @noRd
describe_datetime <- function(x, time_required = TRUE) {
  if (is_iso_datetime(x)) return(NULL)
  if (!time_required && is_iso_date(x)) return(NULL)
  fix <- paste(
    "Write it as YYYY-MM-DD HH:MM:SS (e.g. 2017-07-01 08:00:00).",
    "In Excel or LibreOffice, format the column as Text first, otherwise it is converted again when you save the .csv."
  )

  # 42917.3333: Excel serial number, days (and fraction of a day) since 1899-12-30
  if (grepl("^[0-9]+([.,][0-9]+)?$", x)) {
    n <- as.numeric(sub(",", ".", x))
    guess <- as.POSIXct(round((n - 25569) * 86400), origin = "1970-01-01", tz = "UTC")
    plausible <- guess >= as.POSIXct("2010-01-01", tz = "UTC") && guess <= Sys.time() + 365 * 86400
    return(list(
      problem = paste0(
        sprintf("'%s' is a number, not a date and time. ", x),
        "Spreadsheet programs store dates as a count of days (the decimals are the time of the day), ",
        "and sometimes save that count in the .csv instead of the date",
        if (plausible) sprintf(": it most likely means %s.", format(guess, "%Y-%m-%d %H:%M:%S")) else "."
      ),
      fix = fix
    ))
  }

  parts <- strsplit(trimws(x), "[ T]+")[[1]]
  date_problem <- describe_date(parts[1])
  if (!is.null(date_problem)) return(list(problem = date_problem$problem, fix = fix))
  if (length(parts) == 1) {
    return(list(
      problem = sprintf("'%s' has no time", x),
      fix = sprintf("Add the time when the experiment (ZT0) starts, e.g. %s 08:00:00.", parts[1])
    ))
  }
  time_problem <- describe_time(parts[2])
  if (!is.null(time_problem)) return(list(problem = time_problem$problem, fix = fix))
  if (grepl("^[0-9]:", parts[2]) || grepl("^[0-9]{1,2}:[0-9]{2}$", parts[2])) {
    hms <- as.integer(strsplit(parts[2], ":")[[1]])
    return(list(problem = sprintf("'%s' is not in YYYY-MM-DD HH:MM:SS format", x),
                fix = sprintf("Write it as %s %02d:%02d:%02d.", parts[1], hms[1], hms[2], if (length(hms) > 2) hms[3] else 0L)))
  }
  list(problem = sprintf("'%s' is not a valid date and time", x), fix = fix)
}

describe_time <- function(x) {
  if (grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", x)) return(NULL)
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n) && n >= 0 && n < 1) {
    s <- round(n * 86400)
    guess <- sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
    return(list(
      problem = sprintf("Time '%s' is a fraction of a day, probably changed by Excel. It most likely means %s.", x, guess),
      fix = "Format the column as Text in the spreadsheet and write the time as HH:MM:SS."
    ))
  }
  m <- regmatches(x, regexec("^([0-9]{1,2})[-.]([0-9]{2})[-.]([0-9]{2})$", x))[[1]]
  if (length(m) == 4) {
    return(list(problem = sprintf("Time '%s' uses the wrong separator", x), fix = sprintf("Write it as %s:%s:%s.", m[2], m[3], m[4])))
  }
  list(problem = sprintf("'%s' is not a valid time", x), fix = "Times must be in HH:MM:SS format, e.g. 16:52:43 (the time the ethoscope started recording).")
}

describe_reference_hour <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n) && n >= 0 && n < 24) return(NULL)
  m <- regmatches(x, regexec("^([0-9]{1,2}):([0-9]{2})(:00)?$", x))[[1]]
  if (length(m) >= 3) {
    return(list(
      problem = sprintf("reference_hour '%s' is written as a clock time", x),
      fix = sprintf("Write it as a number of hours: %s.", as.numeric(m[2]) + as.numeric(m[3]) / 60)
    ))
  }
  list(problem = sprintf("reference_hour '%s' is not a number between 0 and 24", x),
       fix = "reference_hour is the hour of the day when the lights turn on (ZT0), e.g. 9 or 9.5.")
}

describe_region_id <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n) && n >= 1 && n == round(n)) return(NULL)
  list(problem = sprintf("region_id '%s' is not a whole number", x),
       fix = "region_id is the number of the tube (ROI) in the ethoscope, from 1 to 20.")
}

# Database ----

#' Compare the metadata against the recordings available in the ethoscope database
#' @noRd
check_against_database <- function(metadata, result_dir) {
  files <- tryCatch(
    suppressWarnings(suppressMessages(scopr::list_result_files(result_dir))),
    error = function(e) conditionMessage(e)
  )
  if (is.character(files)) {
    return(new_problem(
      "error", "", integer(0),
      sprintf("The ethoscope result folder '%s' could not be read: %s", result_dir, files),
      "Check the result folder in the Load tab, and that the app can read it."
    ))
  }
  if (is.null(files) || nrow(files) == 0) {
    return(new_problem(
      "error", "", integer(0),
      sprintf("No ethoscope recordings were found in the result folder '%s'", result_dir),
      "Check the result folder in the Load tab. It should contain one folder per ethoscope, with the .db files inside."
    ))
  }
  files <- data.table::as.data.table(files)[, .(machine_name, date = as.character(date), time = as.character(time))]

  ok <- !is.na(metadata$machine_name) & is_iso_date(metadata$date)
  queries <- unique(data.table::data.table(row = which(ok), machine_name = metadata$machine_name[ok], date = metadata$date[ok]),
                    by = c("machine_name", "date"))
  issues <- lapply(seq_len(nrow(queries)), function(i) {
    q <- queries[i]
    rows <- which(metadata$machine_name == q$machine_name & metadata$date == q$date)
    machine_files <- files[machine_name == q$machine_name]

    if (nrow(machine_files) == 0) {
      known <- unique(files$machine_name)
      similar <- unique(c(
        known[toupper(gsub("[^A-Za-z0-9]", "", known)) == toupper(gsub("[^A-Za-z0-9]", "", q$machine_name))],
        agrep(q$machine_name, known, value = TRUE, max.distance = 2)
      ))
      return(new_problem(
        "error", "machine_name", rows,
        sprintf("There is no ethoscope called '%s' in the database", q$machine_name),
        if (length(similar) > 0) sprintf("Did you mean %s?", paste(head(similar, 3), collapse = " or "))
        else "Names look like ETHOSCOPE_001 (capital letters, 3 digits)."
      ))
    }

    if (!q$date %in% machine_files$date) {
      dates <- sort(unique(machine_files$date))
      nearest <- dates[order(abs(as.numeric(as.Date(dates) - as.Date(q$date))))][1:min(5, length(dates))]
      return(new_problem(
        "error", "date", rows,
        sprintf("%s has no recording that started on %s", q$machine_name, q$date),
        sprintf(
          "'date' is the day the ethoscope started recording. Closest recordings of %s: %s.",
          q$machine_name, paste(sort(nearest), collapse = ", ")
        )
      ))
    }

    if ("time" %in% names(metadata)) {
      times <- metadata$time[rows]
      times <- times[!is.na(times)]
      available <- machine_files[date == q$date, time]
      missing <- setdiff(times, available)
      if (length(missing) > 0) {
        return(new_problem(
          "error", "time", rows[metadata$time[rows] %in% missing],
          sprintf("%s has no recording that started on %s at %s", q$machine_name, q$date, paste(missing, collapse = ", ")),
          sprintf("Recordings that day started at: %s. Leave 'time' empty to use the last one.", paste(available, collapse = ", "))
        ))
      }
    }
    NULL
  })
  data.table::rbindlist(issues)
}

#' First and last date-time recorded in a DAM monitor file
#' @noRd
dam_file_range <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE)
  last <- utils::tail(readLines(path, warn = FALSE), 1)
  parse <- function(line) {
    fields <- strsplit(line, "\t")[[1]]
    # e.g. "30 Jun 17" "14:32:00"; month names are in English
    old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old))
    Sys.setlocale("LC_TIME", "C")
    as.POSIXct(paste(fields[2], fields[3]), format = "%d %b %y %H:%M:%S", tz = "UTC")
  }
  c(parse(first), parse(last))
}

#' Compare a DAM metadata against the monitor files available in the DAM result folder
#' @noRd
check_against_dam_files <- function(metadata, result_dir) {
  if (!dir.exists(result_dir)) {
    return(new_problem(
      "error", "", integer(0),
      sprintf("The DAM result folder '%s' does not exist", result_dir),
      "Check the DAM result folder in the Load tab. It should contain the monitor .txt files."
    ))
  }
  ok <- !is.na(metadata$file)
  issues <- lapply(unique(metadata$file[ok]), function(f) {
    rows <- which(metadata$file == f)
    path <- file.path(result_dir, f)
    if (!file.exists(path)) {
      available <- list.files(result_dir, pattern = "\\.txt$", recursive = TRUE)
      similar <- agrep(basename(f), available, value = TRUE, max.distance = 2)
      return(new_problem(
        "error", "file", rows,
        sprintf("There is no file '%s' in the DAM result folder", f),
        if (length(similar) > 0) sprintf("Did you mean %s? The path is relative to the DAM result folder.", paste(head(similar, 3), collapse = " or "))
        else "Write the name of the monitor file relative to the DAM result folder, e.g. Monitor22.txt."
      ))
    }
    range <- tryCatch(dam_file_range(path), error = function(e) NULL)
    if (is.null(range) || any(is.na(range))) {
      return(new_problem("error", "file", rows, sprintf("'%s' does not look like a DAM monitor file", f),
                         "Check that it is the .txt file written by the DAM system (one line per minute)."))
    }
    start <- suppressWarnings(as.POSIXct(metadata$start_datetime[rows], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    stop_dt <- suppressWarnings(as.POSIXct(substr(metadata$stop_datetime[rows], 1, 19), tz = "UTC",
                                           tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")))
    # swapped start/stop are reported separately
    outside <- which(!is.na(start) & !is.na(stop_dt) & start < stop_dt & (stop_dt <= range[1] | start >= range[2]))
    if (length(outside) > 0) {
      return(new_problem(
        "error", "start_datetime, stop_datetime", rows[outside],
        sprintf("'%s' has no data between start_datetime and stop_datetime", f),
        sprintf("The file has data from %s to %s.", format(range[1], "%Y-%m-%d %H:%M"), format(range[2], "%Y-%m-%d %H:%M"))
      ))
    }
    NULL
  })
  data.table::rbindlist(issues)
}

#' Problems specific to DAM metadata
#' @noRd
check_dam_metadata <- function(metadata, result_dir) {
  problems <- list()
  cols <- names(metadata)
  if ("start_datetime" %in% cols) problems[[length(problems) + 1]] <- problems_by_value(metadata$start_datetime, "start_datetime", describe_datetime)
  if ("stop_datetime" %in% cols) problems[[length(problems) + 1]] <- problems_by_value(metadata$stop_datetime, "stop_datetime", function(x) describe_datetime(x, time_required = FALSE))
  if ("region_id" %in% cols) {
    problems[[length(problems) + 1]] <- problems_by_value(metadata$region_id, "region_id", function(x) {
      n <- suppressWarnings(as.numeric(x))
      if (!is.na(n) && n >= 1 && n <= 32 && n == round(n)) return(NULL)
      list(problem = sprintf("region_id '%s' is not a channel between 1 and 32", x),
           fix = "region_id is the channel (tube) of the DAM monitor, from 1 to 32.")
    })
  } else {
    problems[[length(problems) + 1]] <- new_problem(
      "warning", "region_id", integer(0),
      "There is no region_id column: all 32 channels of each monitor will be loaded with the same metadata",
      "Add a region_id column to describe each fly separately."
    )
  }
  if (all(c("start_datetime", "stop_datetime") %in% cols)) {
    start <- suppressWarnings(as.POSIXct(metadata$start_datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    stop_dt <- suppressWarnings(as.POSIXct(substr(metadata$stop_datetime, 1, 19), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")))
    reversed <- which(!is.na(start) & !is.na(stop_dt) & stop_dt <= start)
    if (length(reversed) > 0) {
      problems[[length(problems) + 1]] <- new_problem("error", "stop_datetime", reversed,
        "stop_datetime is not after start_datetime", "Check that the two columns are not swapped.")
    }
  }
  key <- intersect(c("file", "start_datetime", "region_id"), cols)
  if (length(key) == 3) {
    dup <- which(duplicated(metadata, by = key) | duplicated(metadata, by = key, fromLast = TRUE))
    if (length(dup) > 0) {
      problems[[length(problems) + 1]] <- new_problem("error", paste(key, collapse = ", "), dup,
        "Several rows describe the same fly (same file, start_datetime and region_id)",
        "Check for copy-paste errors in file or region_id.")
    }
  }
  if (!is.null(result_dir) && nzchar(result_dir) && "file" %in% cols) {
    problems[[length(problems) + 1]] <- check_against_dam_files(metadata, result_dir)
  }
  data.table::rbindlist(problems)
}

#' Check a metadata table and explain every problem found
#'
#' @param metadata data.table as returned by read_metadata (all columns as text)
#' @param result_dir optional path to the ethoscope database (or the DAM result folder, for DAM metadata).
#' If given, the metadata is also checked against the recordings available there.
#' @param monitor "ethoscope" or "dam". By default it is told from the columns of the metadata.
#' @return data.table with one row per problem and columns level (error/warning), column,
#' rows (as numbered in a spreadsheet), problem and fix. Empty if the metadata is fine.
#' @export
check_metadata <- function(metadata, result_dir = NULL, monitor = metadata_monitor(metadata)) {
  if (nrow(metadata) == 0) {
    return(new_problem("error", "", integer(0), "The metadata has no rows", "Add one row per fly below the header."))
  }
  if (is.na(monitor)) {
    return(new_problem(
      "error", "", integer(0),
      sprintf("Cannot tell whether this metadata is for ethoscopes or DAM monitors (columns: %s)", paste(names(metadata), collapse = ", ")),
      paste("Ethoscope metadata needs the columns machine_name, date and reference_hour;",
            "DAM metadata needs file, start_datetime and stop_datetime. Use one kind per file.")
    ))
  }

  problems <- list()
  cols <- names(metadata)
  required <- REQUIRED_COLUMNS[[monitor]]

  # Missing columns, suggesting near-misses (e.g. 'Machine name' for machine_name)
  for (col in setdiff(required, cols)) {
    normalise <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
    similar <- cols[normalise(cols) == normalise(col)]
    if (length(similar) == 0) similar <- agrep(col, cols, value = TRUE, max.distance = 2, ignore.case = TRUE)
    problems[[length(problems) + 1]] <- new_problem(
      "error", col, integer(0),
      sprintf("Column '%s' is missing", col),
      if (length(similar) > 0) sprintf("Rename column '%s' to '%s' (column names are case sensitive).", similar[1], col)
      else sprintf("Add a column called '%s'. Columns found: %s.", col, paste(cols, collapse = ", "))
    )
  }

  # Empty cells: scopr silently drops rows with any empty cell
  for (col in cols) {
    empty <- which(is.na(metadata[[col]]))
    if (length(empty) > 0) {
      problems[[length(problems) + 1]] <- new_problem(
        "error", col, empty,
        sprintf("Column '%s' has empty cells", col),
        if (col %in% c(required, "region_id")) "Fill them in, or delete the rows of flies you don't want to load."
        else "Rows with empty cells are not loaded. Write a placeholder such as NONE, or delete the column."
      )
    }
  }

  if (monitor == "dam") {
    problems[[length(problems) + 1]] <- check_dam_metadata(metadata, result_dir)
    out <- data.table::rbindlist(problems)
    if (nrow(out) == 0) return(data.table::data.table(level = character(), column = character(), rows = character(), problem = character(), fix = character()))
    return(out[order(level != "error")])
  }

  checks <- list(date = describe_date, time = describe_time, reference_hour = describe_reference_hour, region_id = describe_region_id)
  for (col in intersect(names(checks), cols)) {
    problems[[length(problems) + 1]] <- problems_by_value(metadata[[col]], col, checks[[col]])
  }

  if (!"region_id" %in% cols) {
    problems[[length(problems) + 1]] <- new_problem(
      "warning", "region_id", integer(0),
      "There is no region_id column: all 20 tubes (ROIs) of each ethoscope will be loaded with the same metadata",
      "Add a region_id column to describe each fly separately."
    )
  }

  key <- intersect(c("machine_name", "date", "time", "region_id"), cols)
  if (all(c("machine_name", "date") %in% key)) {
    dup <- which(duplicated(metadata, by = key) | duplicated(metadata, by = key, fromLast = TRUE))
    if (length(dup) > 0) {
      problems[[length(problems) + 1]] <- new_problem(
        "warning", paste(key, collapse = ", "), dup,
        "Several rows describe the same fly (same ethoscope, date and region_id)",
        "Only the first of them is used. Check for copy-paste errors in machine_name, date or region_id."
      )
    }
    if (!is.null(result_dir) && nzchar(result_dir)) {
      problems[[length(problems) + 1]] <- check_against_database(metadata, result_dir)
    }
  }

  out <- data.table::rbindlist(problems)
  if (nrow(out) == 0) out <- data.table::data.table(level = character(), column = character(), rows = character(), problem = character(), fix = character())
  out[order(level != "error")]
}

#' Find flies that are in the linked metadata but have no tracking data in their .db file
#'
#' The ethoscope lists all 20 ROIs in every recording, but ROIs without a tracked fly
#' (or recordings that were stopped early) have no data. Loading those flies used to crash the app.
#' @param metadata linked metadata, as returned by scopr::link_ethoscope_metadata
#' @return data.table with columns id and reason, one row per fly without data
#' @noRd
flies_without_data <- function(metadata) {
  paths <- sapply(metadata$file_info, function(x) x$path)
  out <- lapply(unique(paths), function(path) {
    rows <- metadata[paths == path]
    tables <- tryCatch({
      con <- RSQLite::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
      on.exit(RSQLite::dbDisconnect(con))
      tbls <- RSQLite::dbListTables(con)
      has_data <- sapply(rows$region_id, function(r) {
        tbl <- paste0("ROI_", r)
        tbl %in% tbls && nrow(RSQLite::dbGetQuery(con, sprintf("SELECT 1 FROM %s LIMIT 1", tbl))) > 0
      })
      data.table::data.table(id = as.character(rows$id[!has_data]))[, reason := "no tracking data for this ROI in the .db file"]
    }, error = function(e) {
      data.table::data.table(id = as.character(rows$id))[, reason := sprintf("the .db file cannot be opened (%s)", conditionMessage(e))]
    })
    tables
  })
  data.table::rbindlist(out)
}

# UI ----

#' Render problems as an HTML table
#' @noRd
problems_table <- function(problems) {
  tags$table(
    class = "fsl-problems",
    tags$thead(tags$tr(tags$th(""), tags$th("Column"), tags$th("Rows"), tags$th("Problem"), tags$th("How to fix it"))),
    tags$tbody(lapply(seq_len(nrow(problems)), function(i) {
      p <- problems[i]
      tags$tr(
        class = paste0("fsl-", p$level),
        tags$td(icon(if (p$level == "error") "times-circle" else "exclamation-triangle"), toupper(p$level)),
        tags$td(tags$code(p$column)), tags$td(p$rows), tags$td(p$problem), tags$td(p$fix)
      )
    }))
  )
}

show_problems_modal <- function(problems, title) {
  showModal(modalDialog(
    title = title, size = "l", easyClose = TRUE,
    problems_table(problems),
    tags$p(tags$small("Row numbers are as shown in Excel: row 1 is the header.")),
    footer = modalButton("Close")
  ))
}


#' Display a data table output of the loaded metadata
#' @noRd
viewMetadataUI <- function(id) {
  ns <- NS(id)
  dataTableOutput(ns("metadata"))
}

viewMetadataServer <- function(id, input_rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output$metadata <- renderDataTable({
        meta <- data.table::copy(behavr::meta(req(input_rv$data)))
        # list columns (file_info) cannot be shown
        meta[, names(meta)[sapply(meta, is.list)] := NULL]
        meta
      })
    }
  )
}
