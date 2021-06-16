context("data score")

test_that("app does not score if data or parameters are not set", {

  input_rv <- reactiveValues(
    ethoscope=reactiveValues(data=NULL,name=NULL,time=NULL),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {

    message("Setting annotation parameters")
    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )
    expect_null(output_rv$ethoscope$data)
  })


  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=Sys.time())

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {
    expect_null(output_rv$ethoscope$data)
  })

})

test_that("app scores when data and parameters are set", {

  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  input_rv <- reactiveValues(
    ethoscope=reactiveValues(data=NULL,name=NULL,time=NULL),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {

    message("Setting annotation parameters")
    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )

    message("Setting input data")
    input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=Sys.time())

    # # let observeEvents react to the invalidation!
    message("Notifying downstream")
    session$flushReact()
    expect_is(output_rv$ethoscope$data, "behavr")
  })
})

test_that("scoring is correct", {

  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  input_rv <- reactiveValues(
    ethoscope=reactiveValues(data=NULL,name=NULL,time=NULL),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {

    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )

    time <- Sys.time()
    input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=time)
    session$flushReact()

    expect_true("asleep" %in% colnames(output_rv$ethoscope$data))
    expect_equal(time, output_rv$ethoscope$time)

    #expect_equal(cnd$message, "Velocity correction coefficient: 0.0048\n")
    scored_data <- session$getReturned()
    expect_identical(scored_data$ethoscope$data, output_rv$ethoscope$data)

    mean_asleep <- mean(output_rv$ethoscope$data$asleep)
    expect_equal(round(mean_asleep, digits = 1), 0.4)
  })
})

test_that("app scores when the dataset time is updated", {

  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  input_rv <- reactiveValues(
    ethoscope=reactiveValues(data=NULL,name=NULL,time=NULL),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  t0 <- Sys.time()

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {

    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )
    input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=t0)
    session$flushReact()

    expect_true(t0 == output_rv$ethoscope$time)

    t1 <- Sys.time()
    input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=t1)
    session$flushReact()
    expect_equal(t1, output_rv$ethoscope$time)
    expect_false(t0 == output_rv$ethoscope$time)
  })
})

test_that("app scores when any scoring parameter changes", {

  dt_raw <- behavr::toy_ethoscope_data()
  data.table::key(dt_raw)
  input_rv <- reactiveValues(
    ethoscope=reactiveValues(data=NULL,name=NULL,time=NULL),
    dam=reactiveValues(data=NULL,name=NULL,time=NULL)
  )

  testServer(scoreDataServer, args = list(input_rv = input_rv, pb=FALSE), {

    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )

    input_rv$ethoscope <- reactiveValues(data=dt_raw, name="toy", time=Sys.time())

    session$flushReact()

    mean_asleep <- mean(output_rv$ethoscope$data$asleep)
    nrows <- nrow(output_rv$ethoscope$data)

    session$setInputs(velocity_correction_coef=0.003)
    session$flushReact()

    mean_asleep2 <- mean(output_rv$ethoscope$data$asleep)
    expect_true(mean_asleep2 < mean_asleep)


    session$setInputs(time_window_length=20)
    session$flushReact()

    nrows2 <- nrow(output_rv$ethoscope$data)
    expect_true(nrows2 < nrows) # the bins are bigger so there are less bins

  })
})


