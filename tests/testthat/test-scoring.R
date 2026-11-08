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

test_that("the masking after an interaction is only passed when changed", {
  testServer(scoringParamsServer, {
    session$setInputs(velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10,
                      curate = TRUE, FUN = "sleep annotation", differentiate_SD = FALSE, masking_duration = 6)
    expect_null(session$returned()$masking_duration)
    session$setInputs(masking_duration = 0)
    expect_equal(session$returned()$masking_duration, 0)
    session$setInputs(differentiate_SD = TRUE, velocity_correction_coef_sd = 0.0048, min_time_immobile_sd = 300,
                      time_window_length_sd = 10, masking_duration_sd = 6)
    expect_equal(session$returned()$masking_duration_sd, 6)
  })
})
