context("snapshot viewer")

shots <- function(n, every_s) {
  data.table::data.table(
    id = seq_len(n), t = (seq_len(n) - 1) * every_s * 1000,
    datetime = as.POSIXct("2026-11-05 19:25:00", tz = "GMT") + (seq_len(n) - 1) * every_s
  )
}

test_that("few snapshots are listed one by one", {
  b <- snapshot_blocks(shots(5, 300))
  expect_equal(b$block, 1:5)
  expect_length(snapshot_choices(b), 5)
})

test_that("long recordings are grouped in 10 minute blocks, including the last one", {
  b <- snapshot_blocks(shots(611, 300)) # a snapshot every 5 minutes for ~51 h
  expect_true(all(b[, .N, by = block]$N <= 2))
  expect_equal(sum(b[, .N, by = block]$N), 611)
  choices <- snapshot_choices(b)
  expect_equal(unname(choices[length(choices)]), as.character(max(b$block)))
  expect_match(names(choices)[1], "^2026-11-05 19:25 \\(2 shots\\)$")
})
