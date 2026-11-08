context("downloads")

test_that("downloads have id, t and a descriptive value column", {
  binned <- data.table::data.table(id = "a", t = c(0, 1800), asleep = c(0.2, 0.4), moving = c(0.8, 0.6), genotype = "wt")
  expect_equal(names(export_table("sleep-binned", binned, y = "asleep", fun = "mean")), c("id", "t", "fraction_of_time_asleep"))
  expect_equal(names(export_table("sleep-binned", binned, y = "moving", fun = "median")), c("id", "t", "median_moving"))
  expect_equal(names(binned), c("id", "t", "asleep", "moving", "genotype")) # the app's data is not modified

  summary <- data.table::data.table(id = "a", genotype = "wt", value = 600, hours = 48)
  data.table::setattr(summary, "value_label", "Sleep per day (min)")
  expect_equal(names(export_table("sleep-summary", summary)), c("id", "sleep_per_day_min"))

  pg <- data.table::data.table(id = "a", period = behavr::hours(c(23, 24)), power = 1:2, signif_threshold = 1)
  out <- export_table("periodogram", pg)
  expect_equal(names(out), c("id", "period_h", "power"))
  expect_equal(out$period_h, c(23, 24))
  expect_false("period_h" %in% names(pg))
})
