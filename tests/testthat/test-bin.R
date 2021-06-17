test_that("binServer works", {

  dt <- behavr::toy_ethoscope_data()
  dt_annot <- sleepr::sleep_annotation(data = dt)

  input_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

  testServer(binDataServer, args = list(input_rv = input_rv), {

    session$setInputs(
      summary_time_window = 30, # unit is min
      summary_FUN = "sleep amount",
      y = "asleep"
    )

    input_rv$data <- dt_annot
    input_rv$name <- "toy"
    # not able to trigger
    session$flushReact()
    expect_null(output_rv$data)
    # setting the time (together with flushReact) should manage to trigger the module
    input_rv$time = Sys.time()
    session$flushReact()
    expect_is(output_rv$data, "data.table") # the module is rejoining the behavr
    mean_sleep <- round(mean(output_rv$data$asleep), 2)
    length_sleep <- as.integer(nrow(output_rv$data))
    sd_sleep <- round(sd(output_rv$data$asleep), 2)

    expect_true(
      all(
        c(mean_sleep, sd_sleep, length_sleep) == c(0.40, 0.31, 240)
      )
    )
  })
})

test_that("binServer can be passed a preprocessing function", {


  dt <- behavr::toy_ethoscope_data()
  dt_annot <- sleepr::sleep_annotation(data = dt)

  input_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

  testServer(binDataServer, args = list(input_rv = input_rv, preproc_FUN = bout_analysis, var="asleep"), {

    session$setInputs(
      summary_time_window = 30, # unit is min
      summary_FUN = "sleep amount",
      y = "duration"
    )

    input_rv$data <- dt_annot
    input_rv$name <- "toy"
    input_rv$time = Sys.time()
    session$flushReact()

    expect_true("duration" %in% colnames(output_rv$data))
    expect_equal(nrow(output_rv$data), 177) # 177 bouts of sleep
    expect_equal(round(mean(output_rv$data$duration / 60), 2), 15.33)
  })
})


test_that("binServer can be passed a preprocessing function and a differnt var", {

  dt <- behavr::toy_ethoscope_data()
  dt_annot <- sleepr::sleep_annotation(data = dt)

  input_rv <- reactiveValues(data = NULL, name = NULL, time = NULL)

  testServer(binDataServer, args = list(input_rv = input_rv, preproc_FUN = bout_analysis, var="moving"), {

    session$setInputs(
      summary_time_window = 30, # unit is min
      summary_FUN = "sleep amount",
      y = "duration"
    )

    input_rv$data <- dt_annot
    input_rv$name <- "toy"
    input_rv$time = Sys.time()
    session$flushReact()

    expect_true(nrow(output_rv$data) == 240)
  })
})
