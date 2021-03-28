library(testthat)
library(data.table)
library(shiny)
library(waldo)

devtools::load_all()
context("data load")

test_that("app loads files as expected", {

  loaded_data <- fslretho::readRDS_behavr(file = file.path(system.file("extdata", package="fslretho"), "raw_data.rds"))
  data.table::key(loaded_data)

  raw_data <- reactiveValues(
    ethoscope = reactiveValues(
      data = loaded_data,
      name = "metadata.csv",
      time = 1
    )
  )


  trigger <- reactiveVal(0)
  testServer(scoreDataServer, args = list("raw_data" = raw_data, "trigger" = trigger), {

    session$setInputs(
      velocity_correction_coef=0.0048,
      min_time_immobile=300,
      time_window_length=10
    )
    # session$flushReact()
    scored_data <- session$getReturned()

    target <- as.data.table(
      readRDS_behavr(file = file.path(system.file("extdata", package="fslretho"), "scored_data.rds"))
    )
    ref <- as.data.table(
      scored_data$ethoscope$data
    )


    min(target$t)
    min(ref$t)
    x <- waldo::compare(target, ref, ignore_attr=TRUE)
    expect_length(x, 0)

  })
})


