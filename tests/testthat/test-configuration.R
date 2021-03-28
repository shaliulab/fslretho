library(testthat)

context("configuration")

expect_that("configuration initialization works as expected", {
  conf <- FSLRethoConfiguration$new()
  expect_is(conf$content, "list")
  expect_true(all(c("debug", "ncores", "stop_backups", "port") %in% names(conf$content)))
})

expect_that("configuration file can be moved elsewhere if not writable", {
  conf <- tryCatch({
    FSLRethoConfiguration$new("/etc/fslretho.conf")
  }, error = function(e) {
    FALSE
  })
  expect_true(R6::is.R6(conf))

})

expect_that("configuration file paths dont get corrupted with duplicates", {
  conf <- FSLRethoConfiguration$new()
  conf <- FSLRethoConfiguration$new()
  conf <- FSLRethoConfiguration$new()
  # this block of code  checks the same folder name
  # is not present more than once on every path
  # i.e. we dont have something like /ethoscope_data/results/ethoscope_data/results/foo
  expect_true(
    all(
    sapply(conf$content$folders, function(f) {
    all((unlist(strsplit(f$path,"/|\\\\")) %>%
    .[.!=""] %>%
      table) == 1)
    })
  ))

})
