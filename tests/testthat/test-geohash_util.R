test_that("decode_bbox decodes a single geohash character correctly", {
  bbox <- decode_bbox("s")
  expect_equal(unname(bbox["lat_min"]), 0)
  expect_equal(unname(bbox["lat_max"]), 45)
  expect_equal(unname(bbox["lon_min"]), 0)
  expect_equal(unname(bbox["lon_max"]), 45)
})

test_that("decode_bbox is case-insensitive and trims whitespace", {
  expect_equal(decode_bbox("s"), decode_bbox("S"))
  expect_equal(decode_bbox("s"), decode_bbox("  s  "))
})

test_that("decode_center returns the midpoint of decode_bbox", {
  center <- decode_center("s")
  expect_equal(unname(center["lat"]), 22.5)
  expect_equal(unname(center["lon"]), 22.5)
})

test_that("bbox_polygon_coords returns a closed ring in lon/lat order", {
  coords <- bbox_polygon_coords("s")
  ring <- coords[[1]]
  expect_length(ring, 5)
  expect_equal(ring[[1]], ring[[5]])
  expect_equal(ring[[1]], c(0, 0))
  expect_equal(ring[[3]], c(45, 45))
})

test_that("decode_bbox rejects invalid geohash characters", {
  expect_error(decode_bbox("s!"))
})
