context("data load")

test_that("app scores when supplying any of the 3 score parameters", {

  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  raw_data <- reactiveValues(
    ethoscope=reactiveValues(data=dt_raw, name="toy", time=0),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  testServer(scoreDataServer, args = list("raw_data" = raw_data), {

    expect_null(monitors_dt$ethoscope$data)

    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )


    expect_is(monitors_dt$ethoscope$data, "behavr")
    expect_true("asleep" %in% colnames(monitors_dt$ethoscope$data))
    #expect_equal(cnd$message, "Velocity correction coefficient: 0.0048\n")
    scored_data <- session$getReturned()
    expect_identical(scored_data$ethoscope$data, monitors_dt$ethoscope$data)

    mean_asleep <- mean(monitors_dt$ethoscope$data$asleep)

    session$setInputs(
      velocity_correction_coef=0.0003
    )

    mean_asleep2 <- mean(monitors_dt$ethoscope$data$asleep)
    expect_true(mean_asleep2 < mean_asleep)
    # the coefficient decreased,
    # so we are less lenient with how much the fly can move and still
    # be considered sleep

    n_rows <- nrow(monitors_dt$ethoscope$data)

    session$setInputs(
      time_window_length=20
    )
    n_rows2 <- nrow(monitors_dt$ethoscope$data)

    expect_true(nrows2 < nrows) # the bins are bigger so there are less bins


  })
})


