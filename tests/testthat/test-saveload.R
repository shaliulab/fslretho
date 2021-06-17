
test_that("saveload module saves and restores sessions", {

  input_rv <- reactiveValues(
    ethoscope = reactiveValues(data = NULL, name = NULL, time = NULL),
    dam = reactiveValues(data = NULL, name = NULL, time = NULL)
  )

  session_filename <- "test"
  conf <- FSLRethoConfiguration$new()
  sessions_folder <- conf$content$folders$ethoscope_sessions$path
  session_path <- file.path(sessions_folder, paste0(session_filename, ".rds"))

  if (file.exists(session_path)) file.remove(session_path)

  testServer(saveLoadSessionServer, args = list(input_rv = input_rv), {

    session$setInputs(`save-button` = 0, `save-path` = "")
    session$setInputs(`load-button` = 0, `load-path` = "")

    input_rv$ethoscope$data <- behavr::toy_ethoscope_data()
    input_rv$ethoscope$name <- "test"
    input_rv$ethoscope$time <- Sys.time()

    # user enters a path
    session$setInputs(`save-path` = session_filename)

    # user presses save
    expect_true(!file.exists(session_path))
    session$setInputs(`save-button` = 1)
    expect_true(file.exists(session_path))
    # the file got saved!

    data <- output_rv$ethoscope$data
    output_rv$ethoscope$data <- NULL
    output_rv$ethoscope$name <- NULL
    output_rv$ethoscope$time <- NULL

    # user selected a dataset
    session$setInputs(`load-path` = "test")
    # user presses load
    expect_true(is.null(output_rv$ethoscope$data))
    session$setInputs(`load-button` = 1)
    expect_is(output_rv$ethoscope$data, "behavr")
    # output_rv$ethoscope$data
    expect_equal(data, output_rv$ethoscope$data)

  })
    file.remove(session_path)
})
