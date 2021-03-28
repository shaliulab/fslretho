library(testthat)
library(shiny)
devtools::load_all()
context("data load")

test_that("app loads files as expected", {

  reload <- reactiveVal(value = 0, label = "reload")

  testServer(loadDataServer, args = list("reload" = reload), {
    metadata <- system.file(
      "extdata/ethoscope_metadata/metadata.csv",
      mustWork = TRUE, package = "fslretho"
    )

    metadata <- list(datapath=metadata)
    results_folder_ethoscope <- system.file(
      "extdata/ethoscope_data/results/", mustWork = TRUE, package = "fslretho"
    )
    results_folder_dam <- system.file(
      "extdata/DAM_data/results/", mustWork = TRUE, package = "fslretho"
    )

    session$setInputs(metadata=metadata, result_dir_ethoscope=results_folder_ethoscope)
    session$setInputs(result_dir_dam=results_folder_dam)
    print(reload())
    reload(1)
    # Sys.sleep(2)
    print(paste0("The test sees ", reload()))
    session$flushReact()
    raw_data <- session$getReturned()
    expect_equal(nrow(raw_data$ethoscope$data), 1000)

    target <- as.data.table(readRDS(system.file("extdata/roi_1.rds", package = "fslretho")))
    ref <- as.data.table(raw_data$ethoscope$data)
    x <- waldo::compare(target, ref, ignore_attr=TRUE)

    # no differences
    expect_length(x, 0)
    expect_equal(raw_data$ethoscope$name,  "metadata.csv")
  })
})


