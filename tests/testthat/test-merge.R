test_that("merge_floating_json concatenates plain-array point lists", {
  f1 <- tempfile(fileext = ".txt")
  f2 <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(c(f1, f2, out)))

  writeLines('[[37.5, 127.0, 10]]', f1, useBytes = TRUE)
  writeLines('[[37.6, 127.1, 20], [37.7, 127.2, 30]]', f2, useBytes = TRUE)

  result <- merge_floating_json(f1, f2, out)
  expect_equal(result$count, 3)

  merged <- jsonlite::read_json(out)
  expect_length(merged, 3)
})

test_that("merge_floating_json keeps the $data wrapper and sibling fields", {
  f1 <- tempfile(fileext = ".txt")
  f2 <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(c(f1, f2, out)))

  writeLines('{"data": [[1, 2, 3]], "meta": "x"}', f1, useBytes = TRUE)
  writeLines('{"data": [[4, 5, 6], [7, 8, 9]]}', f2, useBytes = TRUE)

  result <- merge_floating_json(f1, f2, out)
  expect_equal(result$count, 3)

  merged <- jsonlite::read_json(out)
  expect_length(merged$data, 3)
  expect_equal(merged$meta, "x")
})

test_that("merge_card_json concatenates geohash records under $data", {
  c1 <- tempfile(fileext = ".txt")
  c2 <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(c(c1, c2, out)))

  writeLines('{"data": [{"geohash": "a", "amount": 1}]}', c1, useBytes = TRUE)
  writeLines('{"data": [{"geohash": "b", "amount": 2}, {"geohash": "c", "amount": 3}]}', c2, useBytes = TRUE)

  result <- merge_card_json(c1, c2, out)
  expect_equal(result$count, 3)

  merged <- jsonlite::read_json(out)
  expect_length(merged$data, 3)
  expect_equal(merged$data[[1]]$geohash, "a")
})

test_that("merge_area_json concatenates areas under $contents", {
  a1 <- tempfile(fileext = ".txt")
  a2 <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(c(a1, a2, out)))

  writeLines('{"contents": [{"areaId": "1"}]}', a1, useBytes = TRUE)
  writeLines('{"contents": [{"areaId": "2"}, {"areaId": "3"}]}', a2, useBytes = TRUE)

  result <- merge_area_json(a1, a2, out)
  expect_equal(result$count, 3)

  merged <- jsonlite::read_json(out)
  expect_length(merged$contents, 3)
})

test_that("merge_floating_json defaults out_path to '<dir>/유동인구_merged.txt'", {
  dir <- tempfile()
  dir.create(dir)
  f1 <- file.path(dir, "float1.txt")
  f2 <- file.path(dir, "float2.txt")
  on.exit(unlink(dir, recursive = TRUE))

  writeLines('[[1, 2, 3]]', f1, useBytes = TRUE)
  writeLines('[[4, 5, 6]]', f2, useBytes = TRUE)

  result <- merge_floating_json(f1, f2)
  expect_equal(basename(result$out_path), "유동인구_merged.txt")
  expect_true(file.exists(result$out_path))
})
