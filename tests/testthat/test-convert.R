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

test_that("build_*_geojson default out_path embeds a '<name>_YYMMDDHHMMSS.geojson' timestamp", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  ts_pattern <- "^[0-9]{12}$"

  float_src <- file.path(dir, "float.txt")
  writeLines('[[37.5, 127.0, 1]]', float_src, useBytes = TRUE)
  float_result <- build_floating_geojson(float_src, script_path = NULL)
  float_name <- sub("\\.geojson$", "", basename(float_result$out_path))
  expect_match(float_name, "^유동인구_")
  expect_match(sub("^유동인구_", "", float_name), ts_pattern)

  card_src <- file.path(dir, "card.txt")
  writeLines('{"data": [{"geohash": "s", "amount": 1}]}', card_src, useBytes = TRUE)
  card_result <- build_card_geojson(card_src, script_path = NULL)
  card_name <- sub("\\.geojson$", "", basename(card_result$out_path))
  expect_match(card_name, "^카드매출_")
  expect_match(sub("^카드매출_", "", card_name), ts_pattern)

  area_src <- file.path(dir, "area.txt")
  writeLines('{"contents": [{"areaId": "1", "areaMetaDetail": {"geometry": {"type": "Point", "coordinates": [0, 0]}}}]}',
             area_src, useBytes = TRUE)
  area_result <- build_area_geojson(area_src)
  area_name <- sub("\\.geojson$", "", basename(area_result$out_path))
  expect_match(area_name, "^상권_")
  expect_match(sub("^상권_", "", area_name), ts_pattern)
})
