
test_that("app loads and scores", {

  reload <- reactiveVal(0)
  metadata_path <- file.path(fslretho::fslretho_example_dir(), "ethoscope_metadata.csv")
  dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")
  options("sparse_data" = TRUE)

  testLoadScoreModule <- function(id, ...) {
    moduleServer(id,
                 function(input, output, session) {

                   raw_data <- loadDataServer("load", ...)
                   # loaded_data <- saveLoadServer("sessions", raw_data)
                   scored_data <- scoreDataServer("score", raw_data, pb=FALSE)
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

    # initialize the submit button (needed because there is no GUI)
    session$setInputs(`load-submit`=0)

    # initialize the scoring parameters (needed because there is no GUI)
    session$setInputs(
      `score-velocity_correction_coef`=0.0048,
      `score-min_time_immobile`=300,
      `score-time_window_length`=10
    )


    # (pseudo) upload the metadata, make sure the path to the databse (result_dir_ethoscope) is set
    session$setInputs(`load-metadata`=list(datapath=metadata_path), `load-result_dir_ethoscope`=dir)

    # press submit button
    message("Submit")
    session$setInputs(`load-submit`=1)

    # get the resulting dataset
    metadata <- behavr::meta(scored_data$ethoscope$data)
    data <- data.table::as.data.table(scored_data$ethoscope$data)

    # check the resulting dataset
    expect_true(nrow(metadata) == 20)
    expect_true(nrow(data) == 220)
    expect_true("asleep" %in% colnames(data))
  })
  options("sparse_data" = FALSE)
})




test_that("app allows input from new data and from cached sessoins", {

  reload <- reactiveVal(0)
  metadata_path <- file.path(fslretho::fslretho_example_dir(), "ethoscope_metadata.csv")
  dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")
  options("sparse_data" = TRUE)

  testLoadScoreModule <- function(id, ...) {
    moduleServer(id,
                 function(input, output, session) {

                   raw_data <- loadDataServer("load", ...)
                   loaded_data <- saveLoadServer("sessions", raw_data)
                   return(loaded_data$ethoscope$data)
                 })
  }

  testServer(testLoadScoreModule, args = list("reload" = reload), {


    conf <- fslretho:::FSLRethoConfiguration$new()
    # why: to disable the update progress functionality, which throws warnings while testing without GUI
    conf$content$testing <- TRUE
    # why:
    conf$content$reference_hour_required <- TRUE
    conf$save(conf$config_file)

    # initialize buttons (needed because there is no GUI)
    session$setInputs(`load-submit`=0)
    session$setInputs(`sessions-save` = 0)

    # initialize the scoring parameters (needed because there is no GUI)
    session$setInputs(
      `score-velocity_correction_coef`=0.0048,
      `score-min_time_immobile`=300,
      `score-time_window_length`=10
    )


    # (pseudo) upload the metadata, make sure the path to the databse (result_dir_ethoscope) is set
    session$setInputs(`load-metadata`=list(datapath=metadata_path), `load-result_dir_ethoscope`=dir)
    session$setInputs(`sessions-save-path` = 0)

    # press submit button
    message("Submit")
    session$setInputs(`load-submit`=1)

    # save the dataset
    session$setInputs(`sessions-save-path` = "sparse_dataset.rds")
    browser()
    session$setInputs(`sessions-save-button` = 1)

    # get the resulting dataset
    metadata <- behavr::meta(loaded_data$ethoscope$data)
    data <- data.table::as.data.table(loaded_data$ethoscope$data)

    # check the resulting dataset
    expect_true(nrow(metadata) == 20)
    expect_true(nrow(data) == 220)
    expect_true("asleep" %in% colnames(data))
  })
  options("sparse_data" = FALSE)
})
