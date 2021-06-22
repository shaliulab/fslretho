#' @import RSQLite
sqlite <- function(file, statement) {

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file, flags = RSQLite::SQLITE_RO)

  dt <- tryCatch({
      RSQLite::dbGetQuery(conn = con, statement = statement)
  }, error = function(e) {
    warning("Could not execute SQL query successfully")
    message(e)
  }, finally = function() {
    RSQLite::dbDisconnect(con)
  })

  return(dt)
}
