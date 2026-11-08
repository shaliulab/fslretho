context("loading")

example_results <- function() file.path(scopr::scopr_example_dir(), "ethoscope_results")

upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  list(name = "metadata.csv", datapath = path)
}

default_params <- reactive(list(
  velocity_correction_coef = 0.0048, min_time_immobile = 300, time_window_length = 10,
  # the example recordings are too short to be scored (sleepr needs >= 100 points per fly):
  # test the loading itself, without annotation
  curate = FALSE, FUN = NULL
))

# Link, then load every fly (or only `select`, as sent by the browser)
load_with <- function(metadata_lines, select = NULL) {
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = default_params, parent_session = NULL), {
    session$setInputs(metadata = upload(metadata_lines), result_dir = example_results())
    session$setInputs(link = 1)
    if (!is.null(select)) session$setInputs(selection = list(token = token(), ids = as.list(select)))
    if (!is.null(linked()$linked)) session$setInputs(load = 1)
    result <<- list(data = output_rv$data, problems = problems(), flies = if (!is.null(linked())) flies(), selected = selected())
  })
  result
}

test_that("example data is linked, then loaded", {
  res <- load_with(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7"))
  expect_true(behavr::is.behavr(res$data))
  expect_equal(nrow(behavr::meta(res$data)), 2)
  expect_true(all(c("t", "x") %in% names(res$data)))
  expect_equal(res$flies$status, c("loaded", "loaded"))
  expect_equal(res$flies$region_id, 1:2)
  # the metadata row is not part of the loaded metadata
  expect_false("fsl_row" %in% names(behavr::meta(res$data)))
})

test_that("only the flies selected are loaded", {
  lines <- c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7")
  all <- load_with(lines)
  second <- all$flies[region_id == 2, id]
  res <- load_with(lines, select = second)
  expect_equal(as.character(behavr::meta(res$data)$id), second)
  expect_equal(res$flies[region_id == 1, status], "ok")
  expect_equal(res$flies[region_id == 2, status], "loaded")
})

test_that("a selection sent for another table is ignored", {
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = default_params, parent_session = NULL), {
    session$setInputs(metadata = upload(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7")),
                      result_dir = example_results())
    session$setInputs(link = 1)
    session$setInputs(selection = list(token = token() - 1, ids = list()))
    result <<- selected()
  })
  expect_length(result, 2)
})

test_that("flies without data are listed with the reason, and the others are loaded", {
  res <- load_with(c(
    "machine_name,date,region_id,reference_hour",
    "E_008,2021-06-18,1,9", "E_008,2021-06-18,2,9", # E_008 has no tracking data
    "E_029,2016-01-25,1,7"
  ))
  expect_equal(nrow(behavr::meta(res$data)), 1)
  skipped <- res$flies[status == "error"]
  expect_equal(skipped$fsl_row + 1, 2:3)
  expect_match(skipped$message, "No tracking data for this ROI")
  expect_false(any(skipped$id %in% res$selected))
})

test_that("rows with problems are listed with the problem, and the other rows are loaded", {
  res <- load_with(c("machine_name,date,region_id,reference_hour", "E_029,42394,1,7", "E_029,2016-01-25,2,7"))
  expect_equal(nrow(behavr::meta(res$data)), 1)
  bad <- res$flies[status == "error"]
  expect_equal(bad$fsl_row + 1, 2)
  expect_true(is.na(bad$id))
  expect_match(bad$message, "2016-01-25")
  expect_equal(res$flies[fsl_row == 2, status], "loaded")
})

test_that("nothing is linked when the whole metadata has a problem", {
  res <- load_with(c("machine_name,region_id,reference_hour", "E_029,1,7"))
  expect_null(res$data)
  expect_null(res$flies)
  expect_match(res$problems$problem, "Column 'date' is missing")
})

test_that("duplicated flies are linked once", {
  res <- load_with(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,1,7"))
  expect_equal(nrow(behavr::meta(res$data)), 1)
  expect_match(res$flies[fsl_row == 2, message], "Same fly as row 2")
})

test_that("DAM data is loaded and scored with sleepr::sleep_dam_annotation", {
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = reactive(list(min_time_immobile = 300)), parent_session = NULL), {
    session$setInputs(
      metadata = upload(c("file,start_datetime,stop_datetime,region_id,genotype",
                          "M064.txt,2017-07-01 08:00:00,2017-07-03,1,A", "M064.txt,2017-07-01 08:00:00,2017-07-03,2,B")),
      result_dir = example_results(), result_dir_dam = damr::damr_example_dir()
    )
    session$setInputs(link = 1)
    session$setInputs(load = 1)
    result <<- output_rv$data
  })
  expect_equal(nrow(behavr::meta(result)), 2)
  expect_true(all(c("activity", "moving", "asleep") %in% names(result)))
  expect_true(is_dam_dataset(result))
  expect_equal(max(result$t), behavr::hours(40))
})

test_that("the data available for each fly is listed", {
  res <- load_dataset(
    upload(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7"))$datapath,
    source = "local", remote_dir = "", result_dir = example_results(), result_dir_dam = "",
    params = list(curate = FALSE, FUN = NULL)
  )
  a <- res$flies
  expect_equal(nrow(a), 2)
  expect_true(all(a$rows > 0))
  expect_true(all(a$to > a$from))
  expect_true(all(a$fps > 0))
  expect_equal(a$region_id, 1:2)

  dam <- load_dataset(
    upload(c("file,start_datetime,stop_datetime,region_id", "M064.txt,2017-07-01 08:00:00,2017-07-03,1"))$datapath,
    source = "local", remote_dir = "", result_dir = "", result_dir_dam = damr::damr_example_dir(),
    params = list(min_time_immobile = 300)
  )$flies
  expect_equal(dam$fps, 1 / 60)
  expect_equal(format(dam$from, "%Y-%m-%d %H:%M"), "2017-07-01 08:00")
  expect_equal(dam$status, "loaded")
})

test_that("reload links again and loads the same selection", {
  reload <- reactiveVal(0)
  result <- NULL
  testServer(loadDataServer, args = list(reload = reload, params = default_params, parent_session = NULL), {
    session$setInputs(metadata = upload(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7", "E_029,2016-01-25,2,7")),
                      result_dir = example_results())
    session$setInputs(link = 1)
    second <- linked()$flies$id[2]
    session$setInputs(selection = list(token = token(), ids = list(second)))
    session$setInputs(load = 1)
    first_load <- output_rv$time
    reload(1)
    session$flushReact()
    result <<- list(reloaded = !identical(first_load, output_rv$time), selected = selected(), second = second,
                    loaded = as.character(behavr::meta(output_rv$data)$id))
  })
  expect_true(result$reloaded)
  expect_equal(result$selected, result$second)
  expect_equal(result$loaded, result$second)
})

test_that("new scoring parameters score the loaded flies again", {
  params <- reactiveVal(isolate(default_params()))
  result <- NULL
  testServer(loadDataServer, args = list(reload = reactive(0), params = params, parent_session = NULL), {
    session$setInputs(metadata = upload(c("machine_name,date,region_id,reference_hour", "E_029,2016-01-25,1,7")),
                      result_dir = example_results())
    session$setInputs(link = 1)
    session$setInputs(load = 1)
    first_load <- output_rv$time
    params(modifyList(params(), list(min_time_immobile = 600)))
    session$flushReact()
    result <<- !identical(first_load, output_rv$time)
  })
  expect_true(result)
})
