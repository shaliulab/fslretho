context("downloads")

test_that("downloads have id, t and a descriptive value column", {
  binned <- data.table::data.table(id = "a", t = c(0, 1800), asleep = c(0.2, 0.4), moving = c(0.8, 0.6), genotype = "wt")
  expect_equal(names(export_table("sleep-binned", binned, y = "asleep", fun = "mean")),
               c("id", "t", "fraction_of_time_asleep", "fraction_of_time_moving"))
  # the plotted variable comes first
  expect_equal(names(export_table("sleep-binned", binned, y = "moving", fun = "median")), c("id", "t", "median_moving", "median_asleep"))
  expect_equal(names(binned), c("id", "t", "asleep", "moving", "genotype")) # the app's data is not modified

  # every binned sleep variable, with the interval, whatever is on the Y axis
  full <- data.table::data.table(
    id = "a", interval = c("default", "sd"), t = 0, asleep = 0.5, moving = 0.5, interactions = 2, p_wake = 0.1,
    p_doze = 0.2, n_inactive = 10, n_active = 10, day = 1L, phase = "L", genotype = "wt",
    sleep_min = 15, is_interpolated = 0, sd_on = c(FALSE, TRUE)
  )
  out <- export_table("sleep-binned", full, y = "p_doze", fun = "mean")
  expect_equal(names(out), c("id", "t", "interval", "p_doze", "fraction_of_time_asleep", "sleep_min", "p_wake",
                             "n_inactive", "n_active", "mean_sd_interactions_per_window", "fraction_of_time_moving",
                             "fraction_of_windows_interpolated", "sd_on"))
  # only the variables summarised with the summary function change name
  expect_equal(names(export_table("sleep-binned", full, y = "asleep", fun = "max"))[4:7],
               c("max_asleep", "sleep_min", "p_wake", "n_inactive"))
  expect_equal(out$interval, c("default", "sd"))
  expect_equal(names(export_table("bouts", data.table::data.table(id = "a", t = 0, duration = 60, asleep = TRUE), y = "duration")),
               c("id", "t", "mean_bout_duration_s"))

  summary <- data.table::data.table(id = "a", genotype = "wt", value = 600, hours = 48)
  data.table::setattr(summary, "value_label", "Sleep per day (min)")
  expect_equal(names(export_table("sleep-summary", summary)), c("id", "sleep_per_day_min"))

  pg <- data.table::data.table(id = "a", period = behavr::hours(c(23, 24)), power = 1:2, signif_threshold = 1)
  out <- export_table("periodogram", pg)
  expect_equal(names(out), c("id", "period_h", "power"))
  expect_equal(out$period_h, c(23, 24))
  expect_false("period_h" %in% names(pg))
})
