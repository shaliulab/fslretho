context("loading")

example_results <- function() file.path(scopr::scopr_example_dir(), "ethoscope_results")

upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  list(name = "metadata.csv", datapath = path)
}

default_params <- reactive(list(
  velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10,
  # the example recordings are too short to be scored (sleepr needs >= 100 points per fly):
  # test the loading itself, without annotation
  curate = FALSE, FUN = NULL
))

load_with <- function(metadata_lines) {
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = default_params, parent_session = NULL), {
    session$setInputs(metadata = upload(metadata_lines), result_dir = example_results())
    session$setInputs(submit = 1)
    result <<- list(data = output_rv$data, problems = problems())
  })
  result
}

test_that("example data is loaded", {
  res <- load_with(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7"))
  expect_true(behavr::is.behavr(res$data))
  expect_equal(nrow(behavr::meta(res$data)), 2)
  expect_true(all(c("t", "x") %in% names(res$data)))
})

test_that("flies without data are skipped and reported, instead of crashing", {
  res <- load_with(c(
    "machine_name,date,region_id,reference_hour",
    "E_008,2021-06-18,1,9", "E_008,2021-06-18,2,9", # E_008 has no tracking data
    "E_029,2016-01-25,1,7"
  ))
  expect_equal(nrow(behavr::meta(res$data)), 1)
  skipped <- res$problems[grepl("no data", problem)]
  expect_equal(nrow(skipped), 1)
  expect_match(skipped$problem, "E_008 ROI 1")
  expect_equal(skipped$rows, "2, 3")
})

test_that("nothing is loaded when the metadata has errors", {
  res <- load_with(c("machine_name,date,region_id,reference_hour", "E_029,42394,1,7"))
  expect_null(res$data)
  expect_match(res$problems$problem, "2016-01-25")
})

test_that("DAM data is loaded and scored with sleepr::sleep_dam_annotation", {
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = reactive(list(min_time_immobile = 300)), parent_session = NULL), {
    session$setInputs(
      metadata = upload(c("file,start_datetime,stop_datetime,region_id,genotype",
                          "M064.txt,2017-07-01 08:00:00,2017-07-03,1,A", "M064.txt,2017-07-01 08:00:00,2017-07-03,2,B")),
      result_dir = example_results(), result_dir_dam = damr::damr_example_dir()
    )
    session$setInputs(submit = 1)
    result <<- output_rv$data
  })
  expect_equal(nrow(behavr::meta(result)), 2)
  expect_true(all(c("activity", "moving", "asleep") %in% names(result)))
  expect_true(is_dam_dataset(result))
  expect_equal(max(result$t), behavr::hours(40))
})
