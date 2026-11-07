context("one value per fly")

# 2 days of 30 min bins; asleep the whole dark phase and half of the light phase
binned <- function(id) {
  t <- seq(0, 2 * 86400 - 1800, by = 1800)
  data.table::data.table(id = id, t = t, asleep = ifelse((t %% 86400) >= 43200, 1, 0.5))
}

meta <- data.table::data.table(
  id = c("c", "a", "b"), machine_name = c("ETHOSCOPE_002", "ETHOSCOPE_001", "ETHOSCOPE_001"),
  region_id = c(1L, 2L, 1L), datetime = "2026-11-05 19:25:25"
)

test_that("flies are sorted by date, machine and region", {
  expect_equal(sorted_flies(meta), c("b", "a", "c"))
})

test_that("minutes of sleep over the period, per day and per phase", {
  d <- rbind(binned("a"), binned("b"), binned("c"))
  total <- summarise_flies(d, meta, "asleep", "total")
  expect_equal(levels(total$id), c("b", "a", "c"))
  expect_equal(total$value, rep(2 * (720 + 360), 3)) # 2 days x (12 h dark + 6 h of the light phase)
  expect_equal(summarise_flies(d, meta, "asleep", "per_day")$value, rep(720 + 360, 3))
  expect_equal(summarise_flies(d, meta, "asleep", "D")$value, rep(720, 3))
  expect_equal(summarise_flies(d, meta, "asleep", "L")$value, rep(360, 3))
  expect_true("machine_name" %in% names(total))
})

test_that("other variables are averaged, bout durations in minutes", {
  d <- data.table::data.table(id = "a", t = c(0, 1800, 3600), duration = c(60, 120, 180), p_wake = c(0.1, NA, 0.3))
  expect_equal(summarise_flies(d, meta, "duration")$value, 2)
  expect_equal(summarise_flies(d, meta, "p_wake")$value, 0.2)
})
