library(data.table)
library(RSQLite)
library(fslretho)


# fslretho::make_toy_ethoscope_sqlite(nrows=1e4)

metadata <- data.table::data.table(region_id=1, machine_name="ETHOSCOPE_000", date="2021-03-27", reference_hour=7)
data.table::fwrite(x = metadata, file = "inst/extdata/ethoscope_metadata/metadata.csv")

metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = system.file("extdata/ethoscope_data/results", package = "fslretho"))
raw_data <- scopr::load_ethoscope(metadata = metadata, reference_hour = NA)
fslretho::saveRDS_behavr(object = raw_data, file = "inst/extdata/raw_data.rds")
stopifnot(!is.null(data.table::key(raw_data)))

scored_data <- fslsleepr::sleep_annotation(
  data = raw_data,
  velocity_correction_coef=0.0048,
  min_time_immobile=300,
  time_window_length=10
)

fslretho::saveRDS_behavr(object = scored_data, file = "inst/extdata/scored_data.rds")

con <- RSQLite::dbConnect(RSQLite::SQLite(), list.files(system.file("extdata/ethoscope_data/results", package = "fslretho"), full.names = TRUE, recursive = T),  RSQLite::SQLITE_RO)
data.table::as.data.table(RSQLite::dbGetQuery(con, "SELECT * FROM VAR_MAP"))
RSQLite::dbDisconnect(con)


metadata <- scopr:::metadata
metadata$reference_hour <- 7
data.table::fwrite(metadata, file = "inst/extdata/ethoscope_metadata.csv")

