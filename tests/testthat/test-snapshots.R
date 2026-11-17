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

test_that("the nearest snapshot is found in ZT time", {
  s <- shots(5, 300)
  # the recording started 2 h after ZT0
  offset <- 2 * 3600
  expect_equal(nearest_snapshot(s, offset + 610, offset)$id, 3)
  expect_equal(nearest_snapshot(s, 0, offset)$id, 1)
  expect_equal(nearest_snapshot(s, 1e6, offset)$id, 5)
})

# a jpeg header of a 1280 x 960 frame (SOI, then a SOF0 segment)
fake_jpeg <- as.raw(c(0xFF, 0xD8, 0xFF, 0xC0, 0x00, 0x11, 0x08, 0x03, 0xC0, 0x05, 0x00, rep(0, 20)))

test_that("the size of a jpeg in memory is read", {
  expect_equal(jpeg_size(fake_jpeg), c(1280, 960))
})

test_that("the snapshot preview shows the whole frame or only the ROI of the fly", {
  roi <- data.table::data.table(roi_value = 3, x = 155, y = 366, w = 378, h = 33)
  whole <- as.character(snapshot_preview(fake_jpeg, roi, crop = FALSE, width = 320))
  expect_match(whole, "width: 320px; height: 240px;", fixed = TRUE)
  expect_match(whole, 'viewBox="0 0 1280 960"', fixed = TRUE)
  cropped <- as.character(snapshot_preview(fake_jpeg, roi, crop = TRUE, width = 444, caption = "ZT 1"))
  # the ROI with a margin of one ROI height on each side: 444 x 99 pixels of the frame
  expect_match(cropped, "width: 444px; height: 99px;", fixed = TRUE)
  expect_match(cropped, 'viewBox="122 333 444 99"', fixed = TRUE)
  expect_match(cropped, "left: -122.0px; top: -333.0px; width: 1280.0px", fixed = TRUE)
  expect_match(cropped, "ZT 1")
  # without the ROI of the fly, the whole frame is shown
  expect_match(as.character(snapshot_preview(fake_jpeg, NULL, crop = TRUE)), "height: 240px;", fixed = TRUE)
})
