context("load_metadata")

test_that("load_metadata with ethoscope data works", {
  path_csv <- tempfile()

  df <- data.table(region_id = 1:10, machine_name = "ETHOSCOPE_000", date = "2020-08-02", ZT0 = 12)
  write.csv(df, path_csv, row.names = FALSE)
  metadata <- load_metadata(path_csv, "ethoscope")
  expect_true(TRUE, label = "ethoscope metadata with region_id works")

  df <- data.table(machine_name = "ETHOSCOPE_000", date = "2020-08-02", ZT0 = 12)
  write.csv(df, path_csv, row.names = FALSE)
  metadata <- load_metadata(path_csv, "ethoscope")
  expect_true(TRUE, label = "ethoscope metadata without region_id works")

  df <- data.table(region_id = 1:20, machine_name = "ETHOSCOPE_000", date = "2020-08-02", time = "14:00:00", ZT0 = 12)
  write.csv(df, path_csv, row.names = FALSE)
  metadata <- load_metadata(path_csv, "ethoscope")
  expect_true(TRUE, label = "ethoscope metadata with time column works")
})

test_that("load_metadata with dam data works", {
  path_csv <- tempfile()

  df <- data.table(region_id = 1:10, file = "Monitor00.txt",
                   start_datetime = "2020-08-02 08:00:00", stop_datetime = "2020-08-02")

  write.csv(df, path_csv, row.names = FALSE)
  metadata <- load_metadata(path_csv, "dam")
  expect_true(TRUE, label = "DAM metadata works")

})

test_that("load_metadata with ethoscope invalid warns correctly", {
  path_csv <- tempfile()

  df <- data.table(region_id = 1:20, machine_name = "ETHOSCOPE_000", date = "2020-8-02", time = "14:00:00", ZT0 = 12)
  write.csv(df, path_csv, row.names = FALSE)
  expect_error(load_metadata(path_csv, "ethoscope"), class = "error_bad_argument", label = "ethoscope metadata with invalid date warns the user")

  # Not tested if time is invalid

  df <- data.table(region_id = 1:20, machine_name = "ETHOSCOPE_000", date = "2020-08-02", time = "14:00:00",
                   ZT0 = 12, comment = NA)
  write.csv(df, path_csv, row.names = FALSE)
  expect_error(load_metadata(path_csv, "ethoscope"), class = "error_bad_argument", label = "ethoscope metadata with NA values warns the user")


})

test_that("load_metadata with DAM invalid warns correctly", {

  path_csv <- tempfile()

  df <- data.table(region_id = 1:10, file = "Monitor00.txt",
                   start_datetime = "2020-08-02", stop_datetime = "2020-08-02")
  write.csv(df, path_csv, row.names = FALSE)
  expect_error(load_metadata(path_csv, "dam"), class = "error_bad_argument", label = "dam metadata asks for reference_hour")

  df <- data.table(region_id = 1:10, file = "Monitor00.txt",
                   start_datetime = "2020-08-02 8:00:00", stop_datetime = "2020-08-02")
  write.csv(df, path_csv, row.names = FALSE)
  expect_error(load_metadata(path_csv, "dam"), class = "error_bad_argument", label = "dam metadata asks for reference_hour with format HH:MM:SS")

  df <- data.table(region_id = 1:10, file = "Monitor00.txt",
                   start_datetime = "2020-08-02 8:00:00", stop_datetime = "2020-08-0")
  write.csv(df, path_csv, row.names = FALSE)
  expect_error(load_metadata(path_csv, "dam"), class = "error_bad_argument", label = "dam metadata with invalid date warns the user")

})

# TODO Finish when the documentation of testServer is completed
# https://github.com/rstudio/shiny/issues/2960
# https://shiny.rstudio.com/articles/integration-testing.html
# test_that("metadata interactivity works", {
#
#   module <- function(...) {
#     rv <- reactiveValues(data = df)
#     shinyApp(viewMetadataUI("dummy"), viewMetadataServer(id = "dummy", rv = rv), ...)
#   }
#
#
#   df <- fslbehavr::toy_ethoscope_data()
#
#   shiny::testServer(
#     module(), {
#     expect_equal(output$metadata, data.table(id = "toy_data"))
#   })
#
#
# })
