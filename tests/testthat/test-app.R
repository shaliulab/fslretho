context("interaction")
test_that("fslretho can startup", {
  shinytest::testApp("../../", "startup.R")
  expect_true(TRUE, label = "fslretho starts succesfully")

})

test_that("ethoscope_first.R", {
  shinytest::testApp("../../", "ethoscope.R", compareImages = TRUE)
  expect_true(TRUE, label = "ethoscope.R")
})

# test_that("fslretho can perform basic analysis of ethoscopes", {
#
#   options(shiny.testmode = TRUE)
#   app <- shinytest::ShinyDriver$new("../../")
#   app$snapshotInit("ethoscope")
#   tab_load <- app$findElement('a[href="#shiny-tab-load"]')
#   tab_sleep <- app$findElement('a[href="#shiny-tab-sleep"]')
#   tab_bout <- app$findElement('a[href="#shiny-tab-bout"]')
#   ethoscope_results <- system.file("extdata/ethoscope_results", package = "fslretho")
#
# })

