context("reproducibility")

test_that("the reproducibility script is valid R code", {
  params <- list(
    result_dir = "/ethoscope_data/results",
    scoring = list(velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10, curate = TRUE,
                   velocity_correction_coef_sd = 0.003, min_time_immobile_sd = 300, time_window_length_sd = 10,
                   intervals = list(interval_sd = "SD")),
    summary_time_window = 30, summary_FUN = "median"
  )
  script <- reproducibility_script("metadata.csv", params)
  expect_silent(parse(text = script))
  expect_match(script, "min_time_immobile_sd = 300")
  expect_match(script, "stats::median")
})

test_that("the reproducibility script for DAM data is valid R code", {
  params <- list(monitor = "dam", result_dir = "/DAM_data/results", scoring = list(min_time_immobile = 300),
                 summary_time_window = 30, summary_FUN = "mean")
  script <- reproducibility_script("dam.csv", params)
  expect_silent(parse(text = script))
  expect_match(script, "sleepr::sleep_dam_annotation, min_time_immobile = 300")
  expect_false(grepl("scopr::", script))
})
