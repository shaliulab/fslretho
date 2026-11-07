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

#' Strip the extension from a dataset name so it can be used in a file name
#' @noRd
dataset_basename <- function(name) {
  if (is.null(name) || length(name) == 0 || name == "") return("fslretho")
  tools::file_path_sans_ext(basename(name))
}

#' Run a function of fslretho in a background R process
#'
#' A Shiny app runs in a single R process: while it loads one user's data, every other
#' user has to wait. Long tasks are therefore run in background workers (future package),
#' and only the user who asked for them waits.
#' @param fun_name name of a function of fslretho; it must not use Shiny (no notifications, progress...)
#' @param args list of arguments
#' @param on_done function called with the result, in the user's session
#' @param context description of the task, for error messages
#' @return a promise (to be returned by the observer), or NULL when run synchronously
#' (option fslretho.async = FALSE, or the future package is not installed)
#' @noRd
in_background <- function(fun_name, args, on_done, context) {
  note <- shiny::showNotification(sprintf("Busy %s... Other users of the app are not affected.", context), duration = NULL, type = "message")
  done <- function() shiny::removeNotification(note)
  fail <- function(e) {
    message(sprintf("Error while %s: %s", context, conditionMessage(e)))
    shiny::showNotification(sprintf("Error while %s: %s", context, conditionMessage(e)), type = "error", duration = NULL)
  }

  if (!isTRUE(getOption("fslretho.async", TRUE)) || !requireNamespace("future", quietly = TRUE)) {
    on.exit(done())
    result <- tryCatch(do.call(get(fun_name, envir = asNamespace("fslretho")), args), error = function(e) { fail(e); NULL })
    if (!is.null(result)) on_done(result)
    return(invisible(NULL))
  }

  task <- future::future(
    do.call(get(fun_name, envir = asNamespace("fslretho")), args),
    globals = list(fun_name = fun_name, args = args), packages = "fslretho", seed = TRUE
  )
  p <- promises::then(task, onFulfilled = on_done, onRejected = fail)
  promises::finally(p, done)
}
