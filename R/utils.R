#' Empty container used to pass datasets between modules
#'
#' Every module receives and returns a reactiveValues with three slots:
#' * data: a behavr table (or data.table)
#' * name: the name of the dataset (the metadata file it comes from)
#' * time: when it was last updated. Downstream modules listen to changes in time.
#' @noRd
dataset_rv <- function() {
  shiny::reactiveValues(data = NULL, name = NULL, time = NULL)
}

#' Copy the slots of a dataset into another dataset container
#' @noRd
set_dataset <- function(rv, data, name) {
  rv$data <- data
  rv$name <- name
  rv$time <- Sys.time()
  invisible(rv)
}

#' Run code, and show errors and warnings to the user instead of crashing the session
#'
#' An error inside a Shiny observer disconnects the user (the app turns grey).
#' Wrap the code of observers with this function so the user gets a message instead.
#' @param expr code to run
#' @param context short description of what was being done, shown in the message
#' @param warnings if TRUE, warnings are also shown as notifications
#' @return the value of expr, or NULL if it failed
#' @noRd
safely <- function(expr, context, warnings = FALSE) {
  withCallingHandlers(
    tryCatch(expr, error = function(e) {
      # req() and validate() stop the observer on purpose: let them through
      if (inherits(e, "shiny.silent.error")) stop(e)
      message(sprintf("Error while %s: %s", context, conditionMessage(e)))
      shiny::showNotification(
        sprintf("Error while %s: %s", context, conditionMessage(e)),
        type = "error", duration = NULL
      )
      NULL
    }),
    warning = function(w) {
      if (warnings) shiny::showNotification(conditionMessage(w), type = "warning", duration = 10)
      invokeRestart("muffleWarning")
    }
  )
}

#' Produce a Shiny progress bar
#' @param n Number of steps in the progress bar
#' @return list with the progress object and an update function to call after each step
#' @noRd
get_progress_bar <- function(n, message, ncores = 1) {
  progress <- shiny::Progress$new()
  progress$set(message = message, value = 0)

  update <- function(detail = NULL) {
    if (ncores == 1) {
      progress$inc(amount = 1 / n, detail = detail)
    } else {
      shiny::showNotification(detail, type = "message", duration = 2)
    }
  }
  list(progress = progress, update = update)
}

#' Strip the extension from a dataset name so it can be used in a file name
#' @noRd
dataset_basename <- function(name) {
  if (is.null(name) || length(name) == 0 || name == "") return("fslretho")
  tools::file_path_sans_ext(basename(name))
}
