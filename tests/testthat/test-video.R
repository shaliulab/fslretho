
test_that("video server lists files from metadata", {


  input_rv <- reactiveValues(
    ethoscope = reactiveValues(
    data = NULL,
    name = NULL,
    time = NULL
  ))

  file_info <- list.files(file.path(scopr::scopr_example_dir(), "ethoscope_results", "008"), recursive = TRUE, full.names = TRUE, pattern = ".db$")
  names(file_info) <- file_info
  file_info_short <- substr(basename(file_info), 2, 32)
  names(file_info_short) <- file_info
  file_info <- file_info_short



  x <- data.table(id = "toy")
  data.table::setkey(x, id)

  metadata <- data.table(
    id = "toy",
    file_info = file_info
  )
  data.table::setkey(metadata, id)

  data <- behavr::behavr(
    x = x,
    metadata = metadata
  )


  dbfile <- reactiveVal(file_info)
  trigger <- reactiveVal(0)
  testServer(snapshotViewerServer, args = list(input_rv = input_rv, dbfile=dbfile, trigger=trigger), {

    input_rv$ethoscope$data <- data
    input_rv$ethoscope$name <- "toy"
    input_rv$ethoscope$time <- Sys.time()

    select_ui_html <- select_ui()
    expect_is(select_ui_html, "shiny.tag")

    trigger(1)
    browser()
    summary()

  })
})
