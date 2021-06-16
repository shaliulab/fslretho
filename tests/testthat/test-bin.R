test_that("app chooses ethoscope input source as expected", {

  dt <- behavr::toy_ethoscope_data()
  dt_annot <- sleepr::sleep_annotation(data = dt)
  stopifnot(data.table::key(dt_annot) == "id")

  unified_data <- reactiveValues(
      data = dt_annot,
      name = "metadata.csv",
      time = 1
  )


  testServer(binDataServer, args = list("data" = unified_data, "main"=TRUE), {

    session$setInputs(
      summary_FUN = "sleep amount",
      summary_time_window = 30,
      y = "asleep"
    )


    target <- as.data.table(
      behavr::bin_all(
        loaded_data,
        x = "t",
        y = "asleep",
        x_bin_length = behavr::mins(30),
        FUN = mean
      )
    )

    ref <- as.data.table(
      binned_data$data
    )

    expect_length(x, 0)
  })
})
