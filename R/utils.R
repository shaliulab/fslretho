is.writable <- function(path) {
  # code <- system(paste0("touch ", path, " 2> /dev/null"))
  # if (code == 0)
  #   writable <- TRUE
  # else
  #   writable <- FALSE
  code <- file.access(path, mode=2)
  if (code == 0) writable <- TRUE
  else writable <- FALSE
  return(writable)
}

#' Consistent and informative error
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @param arg The name of the argument not being correct
#' @param must Description of what it should be
#' @param not Description of what it should NOT be
#'
#' Taken from https://adv-r.hadley.nz/conditions.html#signalling-conditions
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  rlang::abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not
  )
}

#
# behavr_placeholder <- function(df = NULL) {
#
#   metadata <- data.table::data.table(id = character())
#   data.table::setkey(metadata, id)
#
#   if (is.null(df)) {
#       x <- data.table::data.table(id = character())
#   } else {
#     x <- data.table::as.data.table(df)
#     x[, id := sample(x = c(1,2), size = nrow(.SD), replace = T)]
#   }
#
#   data.table::setkey(x, id)
#   behavr::behavr(x = x, metadata = metadata)
# }

#' Make a writable behavr object
#' @param data A rejoined behavr or any data.table object
#' @param meta If TRUE, repeat also for the metadata
#' @return A behavr table with no columns of type list
#' TODO Possibly this should be part of a fwrite_behavr function in behavr
#' @importFrom dplyr pull
#' @importFrom purrr map
fortify <- function(data, meta = FALSE) {

  if(meta) {
    metadata <- data[, meta = T]
    metadata <- fortify(metadata, FALSE)
    behavr::setmeta(data, metadata)
  }

  types <- sapply(1:ncol(data), function(i) {
    column_name <- colnames(data)[i]
    res <- c(is.list(data[[i]]))
    names(res) <- column_name
    res
  })

  if (sum(types) != 0) {
    for (column in which(types)) {

      single_column <- dplyr::pull(data[, column, with = F])
      single_column <- purrr::map_chr(single_column, ~.[[1]])
      cmd <- sprintf("%s := single_column", names(types)[column])
      data[, eval(parse(text = cmd))]
    }
  }
  return(data)
}


#' Prepend a filename (or any character) with a datetime signature
#'
#' `datetime_filename` reinforces good documentation practices across
#' fslretho to help users keep track of the files generated with the application
#'
#' @importFrom lubridate year month day hour minute second
#' @importFrom stringr str_pad
datetime_filename <- function(filename) {
  Sys_time <- Sys.time()

  rest <- list(
    month(Sys_time), day(Sys_time),
    hour(Sys_time), minute(Sys_time), floor(lubridate::second(Sys_time))
  )

  date_split <- c(year(Sys_time),
    purrr::map_chr(rest, ~stringr::str_pad(string = ., side = 'left', width = 2, pad = '0'))
  )

  datetime_char <- paste(
    paste0(date_split[1:3], collapse = '-'),
    paste0(date_split[4:6], collapse = '-'),
    sep = '_'
  )
  sprintf("%s_%s", datetime_char, filename)
}


show_condition_message <- function(e, type, session) {
  if (!is.null(session)) {
    shiny::showNotification(
      ui = paste(
        tools::toTitleCase(type),
        conditionMessage(e),
        sep = " : "
      ),
      type = type,
      session = session
    )
  }
}

#' Full copy a reactiveValues object
#' @param x A reactiveValues object
copy_rv <- function(x) {
  y <- reactiveValues()
  for (i in isolate(names(x))) {
    y[[i]] <- isolate(x[[i]])
  }
  y
}

update_rv <- function(rv1, rv2) {
  for (i in isolate(names(rv1))) {
    rv1[[i]] <- isolate(rv2[[i]])
  }
  rv1
}


#' Declare an observer for every data slot in the passed reactive values
#' and map the data and name slots to the passed dest rv upon changes in the data slot
#' of each passed rv
#' @param rv The reactiveValues object to be updated with the contents of the reactiveValues in ...
#' @param ... reactiveValues objects with slots data and name
watch_input <- function(rv, ...) {

  dots <- rlang::list2(...)

  for (module in dots) {
    # stopifnot(!is.null(module$data))
    # stopifnot(!is.null(module$name))
    observeEvent(module$time, {
      req(module$data)
      req(module$name)

      rv$data <- module$data
      rv$name <- module$name
    })
  }

  return(rv)
}

#' Transform the passed rv with slots data and name
#' so the content of data is not a behavr table anymore
#' and instead a rejoin()ed data.table
#'
#' @importFrom tibble as_tibble
#' @importFrom behavr rejoin
#' @param rv A reactiveValues with data and name slots. Data carries a behavr table.
#' @return A reactive values where data is a rejoined behavr i.e. plain data.table.
rejoin_rv <- function(rv) {

  new_rv <- reactiveValues(data = NULL, name = NULL)

  observe({
    req(rv$data)
    new_rv$data <- tibble::as_tibble(behavr::rejoin(rv$data))
    new_rv$name <- rv$name
  })

  return(new_rv)
}


#' Update the path so it is writable for sure
#'
#' If path is not writable, use the same filename but saved instead
#' to the $HOME folder, which should always be writable
#' @param path character for a path in the filesystem
get_writable_path <- function(path) {
  # check if path is not writable
  if (!is.writable(path)) {
    old_path <- path
    dir <- file.path(Sys.getenv("HOME"), ".config")
    path <- file.path(dir, basename(path))
    if (!dir.exists(dir)) dir.create(dir)
    message(paste0("Updating path ", old_path, " -> ", path))
  }
  return(path)
}


