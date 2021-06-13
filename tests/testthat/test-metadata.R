test_that("load metadata reads the scopr internal metadata dataset", {

  metadata <- fslretho::load_metadata(file.path(fslretho::fslretho_example_dir(), "ethoscope_metadata.csv"), monitor = "ethoscope")
  expect_true(nrow(metadata) == 20)
})

