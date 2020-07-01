#' Consistent wayt of returning an error
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
#   fslbehavr::behavr(x = x, metadata = metadata)
# }

#' Make a writable behavr object
#' @param data A rejoined behavr or any data.table object
#' TODO Possibly this should be part of a fwrite_behavr function in behavr
fortify <- function(data) {

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
