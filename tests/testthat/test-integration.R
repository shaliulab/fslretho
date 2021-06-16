
test_that("app loads and scores", {

  reload <- reactiveVal(0)
  metadata_path <- file.path(fslretho::fslretho_example_dir(), "ethoscope_metadata.csv")
  dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")
  options("sparse_data" = TRUE)

  testLoadScoreModule <- function(id, ...) {
    moduleServer(id,
                 function(input, output, session) {

                   input_rv <- loadDataServer("load", ...)
                   scored_data <- scoreDataServer("score", input_rv, pb=FALSE)
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
    # not needed?
    reload(reload() + 1)

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