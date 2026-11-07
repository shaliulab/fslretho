context("metadata")

write_metadata <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

result_dir <- function() file.path(scopr::scopr_example_dir(), "ethoscope_results")

test_that("dates turned into numbers by Excel are explained", {
  d <- describe_date("42394")
  expect_match(d$problem, "is a number, not a date")
  expect_match(d$problem, "2016-01-25")
  expect_match(d$fix, "Text")
  expect_null(describe_date("2016-01-25"))
})

test_that("dates in regional formats are explained", {
  expect_match(describe_date("25/01/2016")$problem, "most likely means 2016-01-25")
  expect_match(describe_date("01/02/2016")$problem, "could mean 2016-02-01 or 2016-01-02")
  expect_match(describe_date("2016/01/25")$fix, "2016-01-25")
  expect_match(describe_date("20160125")$fix, "2016-01-25")
  expect_match(describe_date("2016-01-25 00:00:00")$fix, "2016-01-25")
  expect_match(describe_date("2016-02-30")$problem, "not a valid date")
})

test_that("load_metadata reads the example metadata", {
  metadata <- load_metadata(file.path(fslretho_example_dir(), "ethoscope_metadata.csv"))
  expect_equal(nrow(metadata), 20)
  expect_is(metadata$region_id, "integer")
  expect_is(metadata$date, "character")
})

test_that("a correct metadata has no problems", {
  m <- read_metadata(file.path(fslretho_example_dir(), "ethoscope_metadata.csv"))
  expect_equal(nrow(check_metadata(m, result_dir())), 0)
})

test_that("all problems are reported at once, with spreadsheet row numbers", {
  # Excel's CSV: byte order mark and ; separator
  path <- write_metadata(c(
    "﻿region_id;machine_name;date;reference_hour;genotype",
    "1;E_029;2016-01-25;7;wt",
    "2;E_029;42394;7;wt",
    "3;E-029;2016-01-25;7;wt",
    "4;E_029;2016-01-26;7:30;wt",
    "5;E_029;2016-01-25;7;"
  ))
  p <- check_metadata(read_metadata(path), result_dir())
  expect_true(all(p$level == "error"))
  expect_equal(p[column == "date" & grepl("number", problem), rows], "3")
  expect_match(p[column == "machine_name", fix], "E_029")
  expect_match(p[column == "date" & grepl("no recording", problem), fix], "2016-01-25")
  expect_match(p[column == "reference_hour", fix], "7.5")
  expect_equal(p[column == "genotype", rows], "6")
})

test_that("misspelled column names are spotted", {
  path <- write_metadata(c("Machine name,date,reference_hour", "E_029,2016-01-25,7"))
  p <- check_metadata(read_metadata(path))
  expect_match(p[column == "machine_name", fix], "Rename column 'Machine name'")
})

test_that("flies without tracking data are found before loading", {
  m <- data.table::data.table(machine_name = "E_008", date = "2021-06-18", region_id = 1:2, reference_hour = 9)
  linked <- suppressWarnings(scopr::link_ethoscope_metadata(m, result_dir()))
  missing <- flies_without_data(linked)
  expect_equal(nrow(missing), 2)
})

dam_dir <- function() damr::damr_example_dir()

test_that("the monitor is told from the columns", {
  expect_equal(metadata_monitor(data.table::data.table(machine_name = "E", date = "2021-01-01")), "ethoscope")
  expect_equal(metadata_monitor(data.table::data.table(file = "M064.txt", start_datetime = "x", stop_datetime = "y")), "dam")
  expect_true(is.na(metadata_monitor(data.table::data.table(machine_name = "E", file = "x"))))
})

test_that("DAM metadata problems are explained", {
  path <- write_metadata(c(
    "file,start_datetime,stop_datetime,region_id",
    "M064.txt,2017-07-01 08:00:00,2017-07-03,1",
    "M064.txt,2017-07-01,2017-07-03,2",
    "M064.txt,42917.3333333,2017-07-03,3",
    "M64.txt,2017-07-01 08:00:00,2017-07-03,4",
    "M014.txt,2017-07-01 08:00:00,2017-07-03,40",
    "M014.txt,2018-01-01 08:00:00,2018-01-03,5"
  ))
  p <- check_metadata(read_metadata(path), dam_dir())
  expect_match(p[rows == "3", fix], "08:00:00")
  expect_match(p[rows == "4", problem], "2017-07-01 08:00:00")
  expect_match(p[column == "file", fix], "M064.txt")
  expect_match(p[column == "region_id", problem], "between 1 and 32")
  expect_match(p[rows == "7", fix], "2017-06-30")
})

test_that("correct DAM metadata has no problems", {
  path <- write_metadata(c("file,start_datetime,stop_datetime,region_id", "M064.txt,2017-07-01 08:00:00,2017-07-03,1"))
  expect_equal(nrow(check_metadata(read_metadata(path), dam_dir())), 0)
})
