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

    metadata <- data.table(region_id = 1,
                           machine_name = c("E_014"),
                           date = c("2016-01-25"),
                           time = c("21:46:14"),
                           test=c(1),
                           reference_hour = 10
    )

    temp_file <- upload_metadata(metadata)

    conf <- fslretho:::FSLRethoConfiguration$new()
    conf$content$testing <- TRUE
    conf$content$reference_hour_required <- TRUE
    conf$save(conf$config_file)

    messages <- c()
    withCallingHandlers(
      session$setInputs(metadata=list(datapath=temp_file), result_dir_ethoscope=dir, submit=0),
      message = function(m) {
        messages <<- c(messages, gsub(pattern = "\n", replacement = "", x = m$message))
      }
    )

    expect_true("searching the provided database for data matching query" %in% messages)
    expect_false("Data loaded into R successfully" %in% messages)

    expect_null(output_rv$ethoscope$data)

    messages <- c()
    withCallingHandlers(session$setInputs(submit=1), message = function(m) {
      messages <<- c(messages, gsub(pattern = "\n", replacement = "", x = m$message))
    })
    expect_true("Data loaded into R successfully" %in% messages)

    expect_is(output_rv$ethoscope$data, "behavr")
    expect_true(nrow(output_rv$ethoscope$data) == 56)

    returned <- session$getReturned()
    expect_identical(output_rv$ethoscope$data, returned$ethoscope$data)


    conf$content$testing <- NULL
    conf$save(conf$config_file)
  })
})


test_that("loadDataServer can reload ethoscope data", {

  reload <- reactiveVal(0)

  testServer(loadDataServer, args = list("reload" = reload), {

    dir <- paste0(scopr::scopr_example_dir(), "/ethoscope_results/")

    metadata <- data.table(region_id = 1,
                           machine_name = c("E_014"),
                           date = c("2016-01-25"),
                           time = c("21:46:14"),
                           test=c(1),
                           reference_hour = 10
    )

    temp_file <- upload_metadata(metadata)


    conf <- fslretho:::FSLRethoConfiguration$new()
    # why
    conf$content$testing <- TRUE
    # why
    conf$content$reference_hour_required <- TRUE
    conf$save(conf$config_file)

    session$setInputs(metadata=list(datapath=temp_file), result_dir_ethoscope=dir, submit=0)
    session$setInputs(submit=1)
    first_data <- output_rv$ethoscope$data
    first_time <- output_rv$ethoscope$time

    # make a metadata copy, upload it and check whether the application responds
    metadata$region_id <- 2
    temp_file <- upload_metadata(metadata)
    session$setInputs(metadata=list(datapath=temp_file))

    reload(reload() + 1); session$flushReact()
    # if it reloaded the data, the times and data should be different
    second_time <- output_rv$ethoscope$time
    second_data <- output_rv$ethoscope$data
    expect_false(second_time == first_time)
    expect_false(identical(second_data, first_data))
    expect_true(all(output_rv$ethoscope$data[, meta=T]$region_id == 2))

    # however reloading again nothing to the data
    reload(reload() + 1); session$flushReact()
    expect_identical(output_rv$ethoscope$data, second_data)

    # it still changes the time because Sys.time() is not reactive expression,
    # but a function (i.e. it's not cached)
    expect_false(output_rv$ethoscope$time == second_time)

    conf$content$testing <- NULL
    conf$save(conf$config_file)
  })
})
