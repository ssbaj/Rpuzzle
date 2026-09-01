test_that("load_json_flexible reads plain UTF-8 JSON", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeLines('{"a": 1, "b": [1, 2, 3]}', tmp, useBytes = TRUE)

  data <- load_json_flexible(tmp)
  expect_equal(data$a, 1)
  expect_equal(length(data$b), 3)
})

test_that("run_fetch_script fails gracefully when no script is given", {
  result <- run_fetch_script(NULL)
  expect_null(result$data)
  expect_true(nzchar(result$fail_reason))
})

test_that("build_area_geojson converts area records into a Polygon FeatureCollection", {
  src <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(src, out)))

  writeLines('{
    "contents": [
      {
        "areaId": "A1",
        "areaName": "Test Area",
        "rltm": {"congestion": "여유", "congestionLevel": 1, "datetime": "2026-09-01T00:00:00"},
        "areaMetaDetail": {
          "geometry": {"type": "Polygon", "coordinates": [[[0,0],[1,0],[1,1],[0,1],[0,0]]]},
          "repLat": 37.5,
          "repLng": 127.0,
          "areaM2": 1000
        }
      }
    ]
  }', src, useBytes = TRUE)

  result <- build_area_geojson(src, out)
  expect_equal(result$out_path, out)
  expect_equal(result$count, 1)

  geojson <- jsonlite::read_json(out)
  expect_equal(geojson$type, "FeatureCollection")
  expect_length(geojson$features, 1)
  expect_equal(geojson$features[[1]]$properties$areaName, "Test Area")
})

test_that("build_card_geojson converts geohash records into a Polygon FeatureCollection", {
  src <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(src, out)))

  writeLines('{"data": [{"geohash": "s", "amount": 12345}]}', src, useBytes = TRUE)

  result <- build_card_geojson(src, script_path = NULL, out_path = out)
  expect_equal(result$count, 1)

  geojson <- jsonlite::read_json(out)
  feature <- geojson$features[[1]]
  expect_equal(feature$geometry$type, "Polygon")
  expect_equal(feature$properties$geohash, "s")
  expect_equal(feature$properties$amount, 12345)
})

test_that("build_floating_geojson converts [lat, lng, count] points into a Point FeatureCollection", {
  src <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(src, out)))

  writeLines('[[37.5, 127.0, 42]]', src, useBytes = TRUE)

  result <- build_floating_geojson(src, script_path = NULL, out_path = out)
  expect_equal(result$count, 1)

  geojson <- jsonlite::read_json(out)
  feature <- geojson$features[[1]]
  expect_equal(feature$geometry$type, "Point")
  expect_equal(unlist(feature$geometry$coordinates), c(127.0, 37.5))
  expect_equal(feature$properties$count, 42)
})
