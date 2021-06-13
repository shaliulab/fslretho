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

    nrows <- nrow(monitors_dt$ethoscope$data)

    session$setInputs(
      time_window_length=20
    )
    nrows2 <- nrow(monitors_dt$ethoscope$data)

    expect_true(nrows2 < nrows) # the bins are bigger so there are less bins


  })
})

test_that("app loads and scores", {

  reload <- reactiveVal(0)
  metadata_path <- file.path(fslretho::fslretho_example_dir(), "ethoscope_metadata.csv")
  dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")
  options("sparse_data" = TRUE)

  testLoadScoreModule <- function(id, ...) {
    moduleServer(id,
                 function(input, output, session) {

                   raw_data <- loadDataServer("load", ...)
                   scored_data <- scoreDataServer("score", raw_data)
                   return(scored_data$ethoscope$data)
                 })
  }

  testServer(testLoadScoreModule, args = list("reload" = reload), {

    conf <- fslretho:::FSLRethoConfiguration$new()
    # why: to disable the update progress functionality, which throws warnings while testing without GUI
    conf$content$testing <- TRUE
    # why:
    conf$content$reference_hour_required <- TRUE
    conf$save(conf$config_file)

    session$setInputs(`load-metadata`=list(datapath=metadata_path), `load-result_dir_ethoscope`=dir, `load-submit`=0)
    session$setInputs(`load-submit`=1)
    reload(reload() + 1)
    metadata <- behavr::meta(scored_data$ethoscope$data)
    data <- data.table::as.data.table(scored_data$ethoscope$data)

    expect_true(nrow(metadata) == 20)
    expect_true(nrow(data) == 220)
    expect_true("asleep" %in% colnames(data))
  })
})


