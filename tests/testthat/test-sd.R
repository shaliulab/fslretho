test_that("annotate_sd_daterange works", {

  metadata <- data.table(region_id=1, machine_name="ETHOSCOPE_008", date = "2021-06-18", reference_hour = 5)
  #metadata <- scopr::link_ethoscope_metadata(x = metadata, result_dir = file.path(scopr::scopr_example_dir(), "ethoscope_results"))
  metadata <- scopr::link_ethoscope_metadata(x = metadata, result_dir = "/ethoscope_data/results")
  dt <- scopr::load_ethoscope(metadata)
  metadata <- fortify(metadata)
  behavr::setmeta(dt, metadata)
  # TODO Make dbfile where I can test thi
  dt <- sd_inprogress_annotation(dt)
  table(dt$sd_on)
  expect_true((dt[sd_on == TRUE, t / 3600][1] - 36) < 0.001)
  expect_true((dt[sd_on == TRUE, rev(t) / 3600][1] - 48) < 0.001)
})
