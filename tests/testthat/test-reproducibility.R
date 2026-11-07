context("reproducibility")

test_that("the reproducibility script is valid R code", {
  params <- list(
    result_dir = "/ethoscope_data/results",
    scoring = list(velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10, curate = TRUE,
                   velocity_correction_coef_sd = 0.003, min_time_immobile_sd = 300, time_window_length_sd = 10,
                   intervals = list(interval_sd = "SD")),
    summary_time_window = 30, summary_FUN = "P_doze"
  )
  script <- reproducibility_script("metadata.csv", params)
  expect_silent(parse(text = script))
  expect_match(script, "min_time_immobile_sd = 300")
  expect_match(script, "sleepr::p_doze")
})
