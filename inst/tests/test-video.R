test_server <- function(input, output, session) {


  input_rv <- reactiveValues(
    ethoscope = reactiveValues(
      data = NULL,
      name = NULL,
      time = NULL
    ))

  file_info <- list.files(file.path(scopr::scopr_example_dir(), "ethoscope_results", "008"), recursive = TRUE, full.names = TRUE, pattern = ".db$")
  names(file_info) <- file_info
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

  observe({
    input_rv$ethoscope$data <- data
    input_rv$ethoscope$name <- "toy"
    input_rv$ethoscope$time <- Sys.time()
    print(input_rv$ethoscope$data)
  })


  snapshotViewerServer("viewer", input_rv, dbfile = reactive(file_info))
}

test_ui <- function() {
  snapshotViewerUI("viewer")
}

shinyApp(ui = test_ui, server = test_server)
