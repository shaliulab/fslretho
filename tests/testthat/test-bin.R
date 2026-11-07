context("binning")

scored <- function() {
  dt <- behavr::toy_ethoscope_data(seed = 2, duration = behavr::days(2))
  suppressMessages(sleepr::sleep_annotation(dt))
}

test_that("bin_dataset gives the same result as behavr::bin_apply_all", {
  dt <- scored()
  for (fun in c("mean", "max", "P_doze")) {
    new <- bin_dataset(dt, 30, SUMMARY_FUNCTIONS[[fun]])
    ref <- behavr::bin_apply_all(dt, y = "asleep", x = "t", x_bin_length = behavr::mins(30), FUN = SUMMARY_FUNCTIONS[[fun]])
    expect_true(behavr::is.behavr(new))
    expect_equal(nrow(new), nrow(ref))
    expect_equal(as.data.table(new)[order(t), asleep], as.data.table(ref)[order(t), asleep])
  }
})

test_that("bin_dataset summarises sd_on and the tube position", {
  dt <- scored()
  dt[, sd_on := t > behavr::hours(10) & t < behavr::hours(12)]
  binned <- bin_dataset(dt, 30, mean)
  expect_true(all(c("sd_on", "pareto", "interval") %in% names(binned)))
  expect_is(binned$sd_on, "logical")
  expect_equal(sum(binned$sd_on), 3) # 10:00-10:30 is not entirely under SD
})

test_that("bouts are computed separately in each interval", {
  dt <- scored()
  dt[, interval := ifelse(t > behavr::hours(10) & t < behavr::hours(14), "sd", "default")]
  bouts <- apply_by_interval(dt, bout_analysis)
  expect_setequal(unique(bouts$interval), c("default", "sd"))
  expect_true(all(bouts[interval == "sd", t] > behavr::hours(10) & bouts[interval == "sd", t] < behavr::hours(14)))
  expect_true(behavr::is.behavr(bouts))
})
