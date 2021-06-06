context("load ethoscope")

upload_metadata <- function(metadata) {
  temp_file <- tempfile(pattern = "metadata_", fileext = ".csv")
  data.table::fwrite(x = metadata, file = temp_file, sep = ",", col.names = T)
  return(temp_file)
}


test_that("loadDataServer can load ethoscope data and return it upon submission", {

  reload <- reactiveVal(value = 0, label = "reload")

  testServer(loadDataServer, args = list("reload" = reload), {

    dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")

    metadata <- data.table(machine_name = c("E_014"),
                           date = c("2016-01-25"),
                           time = c("21:46:14"),
                           test=c(1),
                           reference_hour = 10
    )

    temp_file <- upload_metadata(metadata)

    conf <- fslretho:::FSLRethoConfiguration$new()
    conf$content$testing <- TRUE
    conf$content$reference_hour_required <- TRUE
    conf$save()

    session$setInputs(metadata=list(datapath=temp_file), result_dir_ethoscope=dir)

    expect_null(rv$ethoscope$data)
    session$setInputs(submit=1)
    expect_is(rv$ethoscope$data, "behavr")
    expect_true(nrow(rv$ethoscope$data) == 271)

    returned <- session$getReturned()
    expect_identical(rv$ethoscope$data, returned$ethoscope$data)


    conf$content$testing <- NULL
    conf$save()
  })
})


test_that("loadDataServer can reload ethoscope data", {

  reload <- reactiveVal(value = 0, label = "reload")

  testServer(loadDataServer, args = list("reload" = reload), {

    dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")

    metadata <- data.table(machine_name = c("E_014"),
                           date = c("2016-01-25"),
                           time = c("21:46:14"),
                           test=c(1),
                           reference_hour = 10
    )

    temp_file <- upload_metadata(metadata)


    conf <- fslretho:::FSLRethoConfiguration$new()
    conf$content$testing <- TRUE
    conf$content$reference_hour_required <- TRUE
    conf$save()

    session$setInputs(metadata=list(datapath=temp_file), result_dir_ethoscope=dir)
    session$setInputs(submit=1)
    first_time <- rv$ethoscope$time

    # make a metadata copy, upload it and check whether the application responds
    metadata$test <- 2
    temp_file <- upload_metadata(metadata)
    session$setInputs(metadata=list(datapath=temp_file))
    reload(reload() + 1); session$flushReact()

    expect_false(rv$ethoscope$time == first_time)
    expect_true(all(rv$ethoscope$data[, meta=T]$test == 2))

    conf$content$testing <- NULL
    conf$save()
  })
})