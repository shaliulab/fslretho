#' Create header of table to store data of 1 fly
#' @importFrom RSQLite dbExecute
create_roi_table <- function(conn) {
  RSQLite::dbExecute(conn = conn,
                     statement = "CREATE TABLE ROI_1 (id int(11) PRIMARY KEY, t int(11), xy_dist_log10x1000 smallint(6), has_interacted smallint(6), x smallint(6));")
}

#' Generate fake data for a fly
#' @importFrom RSQLite dbExecute
#' @importFrom glue glue
populate_table <- function(conn, data_table, table_name, every=100, id=TRUE) {

  i <- 1
  populate_row <- function(row) {

    header <- paste(names(row), collapse=",")
    if (id) header <- paste0("id,", header)
    values <- paste(row, collapse=",")
    if (id) values <- paste0(i, ",", values)

    statement <- glue::glue("INSERT INTO {table_name} ({header}) VALUES ({values});")
    message(statement)
    RSQLite::dbExecute(conn = conn, statement = statement)

    if (i %% every == 0) {
      message(paste0("Writing ", i,"th row of ", table_name, " table"))
    }
    i <<- i + 1

  }
  apply(data_table, 1, populate_row)
}


#' Generate data for a fake fly and save it to a SQLite table
#' @importFrom RSQLite dbConnect dbDisconnect
populate_roi <- function(dbfile_path, table, ...) {

  mydb <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile_path)

  create_roi_table(mydb)
  populate_table(mydb, table, "ROI_1", ...)
  RSQLite::dbDisconnect(mydb)
}


populate_metadata <- function(dbfile_path, metadata) {
  mydb <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile_path)
  statement <- "CREATE TABLE METADATA (field char(100), value varchar(4000));"
  RSQLite::dbExecute(conn = mydb, statement = statement)
  populate_table(mydb, metadata, "METADATA", every = 99, id=FALSE)
  RSQLite::dbDisconnect(mydb)

}

populate_var_map <- function(dbfile_path, var_map) {
  mydb <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile_path)
  statement <- "CREATE TABLE VAR_MAP (var_name char(100), sql_type char(100), functional_type char(100));"
  RSQLite::dbExecute(conn = mydb, statement = statement)
  populate_table(mydb, var_map, "VAR_MAP", every = 99, id=FALSE)
  RSQLite::dbDisconnect(mydb)

}



populate_roi_map <- function(dbfile_path, roi_map) {
  mydb <- RSQLite::dbConnect(RSQLite::SQLite(), dbfile_path)
  statement <- "CREATE TABLE ROI_MAP (roi_idx smallint(6), roi_value smallint(6), x smallint(6), y smallint(6), w smallint(6), h smallint(6))"
  RSQLite::dbExecute(conn = mydb, statement = statement)
  populate_table(mydb, roi_map, "ROI_MAP", every = 99, id=FALSE)
  RSQLite::dbDisconnect(mydb)
}


get_dbfile_path <- function(results_folder, machine_id, machine_name, datetime) {
  # Prepare dbfile path
  db_folder <- file.path(results_folder, machine_id, machine_name, datetime)
  dir.create(db_folder, recursive=TRUE, showWarnings = F)
  dbfile_path <- file.path(db_folder, paste0(datetime, "_", machine_id, ".db"))
  return(dbfile_path)
}

#' @importFrom fslbehavr toy_ethoscope_data
#' @importFrom dplyr select
#' @export
write_sqlite <- function(raw_data, var_map, metadata, roi_map, dbfile_path=NULL, nrows = 1e5, temp=FALSE, ...) {
  set.seed(2021)

  if (is.null(dbfile_path)) {
    results_folder <- ifelse(temp, tempdir(), system.file("inst/extdata/ethoscope_data/results/", package = "fslretho"))
    dbfile_path <- get_dbfile_path(results_folder, machine_id, machine_name, datetime) # creates a new one on the spot
  }

  if (file.exists(dbfile_path)) {file.remove(dbfile_path)}

  code <- tryCatch({
    populate_var_map(dbfile_path, var_map)
    populate_metadata(dbfile_path, metadata)
    populate_roi_map(dbfile_path, roi_map)
    populate_roi(dbfile_path, raw_data, ...)
    # success
    0
  }, error = function(e) {
    # fail
    message(e)
    1
  })
  return(code)
}

#' A function to create a sqlite3 file under the extdata of the package
#' @export
#' @importFrom data.table data.table
#' @importFrom dplyr select
#' @importFrom fslbehavr toy_ethoscope_data
make_toy_ethoscope_sqlite <- function(nrows=1e4) {

  set.seed(2021)

  # simulate a machine name machine id and datetime
  machine_id <- paste0("000", paste(sample(c(0:9, letters[1:6]), size = 32-3, replace = TRUE), collapse=""))
  machine_name <- "ETHOSCOPE_000"
  datetime <-  "2021-03-27_17-00-00"

  # obtain the resulting path
  dbfile <- get_dbfile_path(
    results_folder = "inst/extdata/ethoscope_data/results/",
    machine_id = machine_id,
    machine_name = machine_name,
    datetime = datetime
  )

  # create the ROI_1 table as da.table
  raw_data <- fslbehavr::toy_ethoscope_data()
  raw_data$t <- raw_data$t * 1000
  raw_data <- raw_data %>% dplyr::select(., t:x)
  raw_data <- raw_data[1:nrows]


  quote_char <- function(x) {
    paste0('"', x, '"')
  }

  # create the METADATA table as data.table
  frame_width <- 1280
  frame_height <- 960
  date_time <- 1

  metadata <- data.table(
    field = c("machine_id", "machine_name", "date_time", "frame_width", "frame_height", "version", "experimental_info", "selected_options", "stop_date_time") %>% quote_char,
    value = c(machine_id, machine_name, date_time, frame_width, frame_height, paste0(rep(0, 32), collapse = ""), "{}", "{}", 2)  %>% quote_char
  )

  # create the ROI_MAP table as a data.table
  roi_map <- data.table(
    roi_idx = 1, roi_value = 1, x = 0, y = 0, w = 1, h = 1
  )

  # create the VAR_MAP table as a data.table
  var_map <- data.table::data.table(
    var_name = c("x", "xy_dist_log10x1000", "has_interacted")  %>% quote_char,
    sql_type = c("SMALLINT", "SMALLINT", "SMALLINT")  %>% quote_char,
    functional_type = c("distance", "relative_distance", "interaction")  %>% quote_char
  )

  # write this to a sqlite3 file
  write_sqlite(raw_data, var_map, metadata, roi_map,
               dbfile_path = dbfile, every=1e4)

}

