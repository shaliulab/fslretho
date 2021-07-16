#' @import RSQLite
sqlite_statement <- function(file, statement, flags=RSQLite::SQLITE_RO, verbose=F) {

  message("Opening connection")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file, flags = flags)

  # browser()
  error <- FALSE
  dt <- tryCatch({
    message("Executing statement")
    if (verbose) {
      print(statement)
    }
    RSQLite::dbBegin(con)
    RSQLite::dbExecute(conn = con, statement = statement)
    RSQLite::dbCommit(con)
  }, error = function(e) {
    warning("Could not execute SQL statement successfully")
    warning(e)
    error <<- TRUE
  })

  message("Closing connection")
  RSQLite::dbDisconnect(con)
  return(dt)
}
