`%||%` <- function(a, b) if (is.null(a)) b else a

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
      if (!nzchar(conditionMessage(e))) {
        # nothing useful to tell the user; keep the details in the log
        message(sprintf("Error without message while %s: class %s, call %s", context,
                        paste(class(e), collapse = "/"), paste(deparse(conditionCall(e)), collapse = " ")))
        return(NULL)
      }
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

#' Report the progress of a background task (see in_background)
#'
#' The task runs in another R process, so it writes its progress to a file that the app reads.
#' @param file progress file given by in_background (NULL: nothing is reported)
#' @noRd
report_progress <- function(file, message, detail = NULL, value = NULL) {
  if (is.null(file)) return(invisible(NULL))
  # one temporary file per process: flies may be loaded by several processes at the same time
  tmp <- paste0(file, ".", Sys.getpid(), ".tmp")
  saveRDS(list(message = message, detail = detail, value = value), tmp)
  file.rename(tmp, file)
  invisible(NULL)
}

#' Run a function of fslretho in a background R process
#'
#' A Shiny app runs in a single R process: while it loads one user's data, every other
#' user would have to wait. Long tasks are therefore run in background workers (future package),
#' and only the user who asked for them waits.
#' @param fun_name name of a function of fslretho; it must not use Shiny (no notifications, progress...)
#' @param args list of arguments
#' @param on_done function called with the result, in the user's session
#' @param context description of the task, e.g. "loading the data", shown while it runs and in error messages
#' @param on_failed function called (without arguments) if the task fails
#' @param progress if TRUE, show a progress bar, updated by the task: it gets a progress_file argument
#' to pass to report_progress
#' @return NULL, invisibly. The promise is not returned on purpose: an observer returning a promise
#' makes Shiny hold every message to the user (progress included) until it is resolved.
#' @noRd
in_background <- function(fun_name, args, on_done, context, progress = FALSE, on_failed = function() NULL) {
  label <- paste0(toupper(substr(context, 1, 1)), substring(context, 2), "...")
  fail <- function(e) {
    on_failed()
    message(sprintf("Error while %s: %s", context, conditionMessage(e)))
    shiny::showNotification(sprintf("Error while %s: %s", context, conditionMessage(e)), type = "error", duration = NULL)
  }
  async <- isTRUE(getOption("fslretho.async", TRUE)) && requireNamespace("future", quietly = TRUE)

  if (progress && async) {
    progress_file <- tempfile(fileext = ".rds")
    args$progress_file <- progress_file
    bar <- shiny::Progress$new()
    bar$set(message = label, value = 0)
    # follow the progress reported by the task
    poll <- shiny::observe({
      shiny::invalidateLater(500)
      state <- tryCatch(readRDS(progress_file), error = function(e) NULL)
      # an empty detail must be sent explicitly, otherwise the previous one stays
      if (!is.null(state)) bar$set(message = state$message, detail = state$detail %||% "", value = state$value)
    })
    done <- function() {
      poll$destroy()
      bar$close()
      unlink(progress_file)
    }
  } else {
    note <- shiny::showNotification(label, duration = NULL, type = "message")
    done <- function() shiny::removeNotification(note)
  }

  if (!async) {
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
  invisible(NULL)
}
