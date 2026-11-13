context("binning")

scored <- function() {
  dt <- behavr::toy_ethoscope_data(seed = 2, duration = behavr::days(2))
  suppressMessages(sleepr::sleep_annotation(dt))
}

test_that("bin_dataset gives the same result as behavr::bin_apply_all", {
  dt <- scored()
  for (fun in c("mean", "max", "median")) {
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

test_that("P(Wake) and P(Doze) follow Wiggin et al. 2020", {
  # minutes: inactive, inactive, active, inactive, active, active
  active <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  # inactive minutes except the last minute: 3, of which 2 are followed by an active minute
  expect_equal(leave_probability(active, FALSE), list(p = 2 / 3, n = 3L))
  # active minutes except the last minute: 2, of which 1 is followed by an inactive minute
  expect_equal(leave_probability(active, TRUE), list(p = 1 / 2, n = 2L))
  # no inactive minute: undefined
  expect_true(is.na(leave_probability(c(TRUE, TRUE), FALSE)$p))
})

test_that("P(Wake) is computed on 1 minute activity and left empty with too few minutes", {
  # 10 s windows: moving only in the first window of minutes 3 and 6
  d <- data.table::data.table(id = "a", interval = "default", t = seq(0, 3590, by = 10))
  d[, moving := t %in% c(120, 300)]
  p <- transition_probabilities(d, bin_length = 3600, min_minutes = 0)
  # 60 minutes, active at minutes 2 and 5: inactive minutes before the last = 57, 2 followed by activity
  expect_equal(p$n_inactive, 57L)
  expect_equal(p$p_wake, 2 / 57)
  expect_true(is.na(transition_probabilities(d, 3600, min_minutes = 5)$p_doze)) # only 2 active minutes
})

test_that("P(Wake) and P(Doze) do not count transitions across missing minutes", {
  # minutes 0, 1, 3, 4 (minute 2 missing, e.g. removed by curation): inactive, inactive, active, active
  active <- c(FALSE, FALSE, TRUE, TRUE)
  minute <- c(0, 60, 180, 240)
  # minute 1 is not followed by minute 2: only minute 0 is an opportunity, and it stays inactive
  expect_equal(leave_probability(active, FALSE, minute), list(p = 0, n = 1L))
  # without the times, minute 1 -> 3 would be counted as a wake transition
  expect_equal(leave_probability(active, FALSE), list(p = 1 / 2, n = 2L))

  d <- data.table::data.table(id = "a", interval = "default", t = seq(0, 290, by = 10))
  d[, moving := t >= 180]
  d <- d[!(t >= 120 & t < 180)] # remove minute 2
  p <- transition_probabilities(d, bin_length = 300, min_minutes = 0)
  expect_equal(p$p_wake, 0)
  expect_equal(p$n_inactive, 1L)
})

test_that("binned data has p_wake and p_doze", {
  binned <- bin_dataset(scored(), 30, mean)
  expect_true(all(c("p_wake", "p_doze", "n_inactive", "n_active") %in% names(binned)))
  expect_true(all(binned$p_wake >= 0 & binned$p_wake <= 1, na.rm = TRUE))
})

test_that("sleep_min counts the minutes asleep, not the missing data", {
  dt <- scored()
  binned <- data.table::as.data.table(bin_dataset(dt, 30, mean))
  expect_equal(binned$sleep_min, binned$asleep * 30) # full bins
  # the same with median: sleep_min does not depend on the summary function
  expect_equal(data.table::as.data.table(bin_dataset(dt, 30, stats::median))$sleep_min, binned$sleep_min)
  # half of a bin missing
  gap <- data.table::as.data.table(bin_dataset(dt[!(t >= behavr::hours(1) & t < behavr::hours(1.25))], 30, mean))
  full <- dt[t >= behavr::hours(1.25) & t < behavr::hours(1.5)]
  expect_equal(gap[t == behavr::hours(1), sleep_min], sum(full$asleep) * 10 / 60)
  # not for bouts (one row per bout)
  bouts <- bin_dataset(apply_by_interval(dt, bout_analysis), 30, mean, sleep_minutes = FALSE)
  expect_false("sleep_min" %in% names(bouts))
})

test_that("binned data has the day and phase of each bin", {
  binned <- data.table::as.data.table(bin_dataset(scored(), 30, mean))
  expect_equal(binned[t == behavr::hours(0), unique(day)], 1L)
  expect_equal(binned[t == behavr::hours(23.5), c(unique(day), unique(phase))], c("1", "D"))
  expect_equal(binned[t == behavr::hours(24), c(unique(day), unique(phase))], c("2", "L"))
  expect_equal(binned[t == behavr::hours(36), unique(phase)], "D")
})
