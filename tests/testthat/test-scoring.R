context("scoring parameters")

test_that("SD parameters are only passed when requested", {
  testServer(scoringParamsServer, {
    session$setInputs(
      velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10,
      curate = TRUE, FUN = "sleep annotation", differentiate_SD = FALSE,
      velocity_correction_coef_sd = 0.003, min_time_immobile_sd = 60, time_window_length_sd = 10
    )
    p <- session$returned()
    expect_null(p$intervals)
    expect_null(p$min_time_immobile_sd)
    expect_identical(p$FUN, list(sleepr::sleep_annotation))

    session$setInputs(differentiate_SD = TRUE)
    p <- session$returned()
    expect_equal(p$intervals, list(interval_sd = "SD"))
    expect_equal(p$min_time_immobile_sd, 60)
  })
})
