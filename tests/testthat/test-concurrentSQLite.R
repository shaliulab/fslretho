library(parallel)
library(RSQLite)
library(magrittr)

use_original_file <- function(file) return(file)
use_temporary_copy <- function(file) {
  file_temp <- tempfile(fileext = ".db")
  message(paste0("Copying ", file, " to ", file_temp))
  stopifnot(file.exists(file))
  file.copy(file, file_temp)
  return(file_temp)
}

tryOpen <- function(con) {
  data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM ROI_1;"))
}

test <- function(dbfile_path, i=0, count=0,max_count=3, copy=TRUE) {

  if (i %% 100 == 0) {
    message(paste0("Running ", i,"th test"))
  }

  if (copy) {
    FILE <- use_temporary_copy(dbfile_path)
  } else {
    FILE <- dbfile_path
  }

  code <- tryCatch({
    connected <- FALSE
    mydb <- RSQLite::dbConnect(RSQLite::SQLite(), FILE, flags = RSQLite::SQLITE_RO, timeout=5)
    connected <- TRUE
    tryOpen(mydb)
    0
  }, error = function(e) {

    message(glue::glue("Retrial {count}/{max_count} did not work!"))
    print(paste(FILE, e))
    # print(connected)
    if (count < max_count) {
      code <- test(dbfile_path=dbfile_path, count=count+1, copy=TRUE)
      if (code == 0) message(glue::glue("Retrial {count+1} works!"))
      return(code)
    } else {
      return(count+1)
    }
  }, finally = {
    if (connected) RSQLite::dbDisconnect(mydb)
  })

  return(code)
}


test_that("even if database is locked, copying the .db file to a temporary file and opening the copy always works!", {


  ntests <- 200
  nrows <- 5000

  FUN = function(i) {

    dbfile <- get_dbfile_path(results_folder = tempdir())
    message(dbfile)

    if (i == 1) {
      # write a sqlite file
      status <- write_sqlite(nrows = nrows, dbfile_path = dbfile, every=100)
      message("Done")
      # if status == 0 -> success, otherwise error
      status
    } else if (i == 2) {
      # give some time for the first data to be written
      Sys.sleep(2)
      # test
      test_result <- sapply(1:ntests, function(i) test(dbfile_path = dbfile, i=i))
      return(test_result)
      # nfails <- sum(test_result)
      # if nfails == 0 -> no errors or at least successfully handled
      # nfails
    }
  }

  if (!testthat::is_testing()) {
    write_file <- tempfile(fileext = ".txt")
    file.create(write_file)
    print(write_file)
    system(paste0("terminator -e 'tail -f ", write_file, "'"))
    con <- file(write_file, "w")
    sink(file = con, type = "message")
  }

  results <- parallel::mclapply(X = 1:2, FUN = FUN, mc.cores = 2)
  sink(type="message")

  # if results[[1]] is 0, the file could be written to without a problem
  # if results[[2]] is 0, the file could be opened every time or at least retried with success
  expect_true(results[[1]] == 0 & sum(results[[2]]) == 0)
})

