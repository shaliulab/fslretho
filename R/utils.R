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
#' @importFrom rlang abort
#'
#' @param arg The name of the argument not being correct
#' @param must Description of what it should be
#' @param not Description of what it should NOT be
#'
#' Taken from https://adv-r.hadley.nz/conditions.html#signalling-conditions
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- paste0("`", arg, "` must ", must)
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- paste0(msg, "; not ", not, ".")
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
    lapply(rest, function(x) stringr::str_pad(string = x, side = 'left', width = 2, pad = '0'))
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
#' @importFrom behavr rejoin
#' @param rv A reactiveValues with data and name slots. Data carries a behavr table.
#' @return A reactive values where data is a rejoined behavr i.e. plain data.table.
rejoin_rv <- function(rv) {

  new_rv <- reactiveValues(data = NULL, name = NULL)

  observe({
    req(rv$data)
    new_rv$data <- behavr::rejoin(rv$data)
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


#' Produce a Shiny progress bar
#' @param n Number of steps in the progress bar i.e. the number of times
#' the progress bar needs to be advanced by 1 to have it completed
#' @importFrom shiny showNotification
get_progress_bar <- function(n, message, duration=2, ncores=1) {

  rv <- reactiveValues(progress = NULL, update = NULL)
  rv$progress <- shiny::Progress$new()
  rv$progress$set(message = message, value = 0)
  # on.exit(progress$close())

  rv$update <- function(detail = NULL) {
    if (ncores == 1) {
      rv$progress$inc(amount = 1 / n, detail = detail)
    } else {
      showNotification(detail, type = "message", duration = duration)
    }
  }

  # return(update)
  return(rv)
}



#' Incorporate daterange information to a behavr table
#' @param dt A behavr table
#' @return The same behavior table where the metadaata now features columns start_sd and end_sd
#' which contain the number of ms since experiment start until SD start and end
parse_sd_daterange <- function(dt) {

  meta <- behavr::meta(dt)
  # be careful! this c() is coercing the stuff in . to a character
  # . are numbers that are now becoming characters silently
  timestamp_daterange <- 1:nrow(meta) %>% lapply(., function(i) scopr::load_sd_daterange(meta[i, ,drop=F]) %>% c(meta[i, as.character(id)], .)) %>%
    do.call(rbind, .) %>%
    as.data.table

  colnames(timestamp_daterange) <- c('id', "start_sd", "end_sd")
  setkey(timestamp_daterange, id)
  meta <- meta[timestamp_daterange]

  meta$start_sd <- as.numeric(meta$start_sd)
  meta$end_sd <- as.numeric(meta$end_sd)

  behavr::setmeta(dt, meta)
  dt
}

