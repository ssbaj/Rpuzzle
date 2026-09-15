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

test_that("build_card_geojson regrids geohash records onto a fixed cell_size square grid (matching build_floating_geojson)", {
  card_src <- tempfile(fileext = ".txt")
  card_out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(card_src, card_out)))

  writeLines('{"data": [
    {"geohash": "wydm9", "amount": 100},
    {"geohash": "wydm9", "amount": 50},
    {"geohash": "wydmc", "amount": 30}
  ]}', card_src, useBytes = TRUE)

  cell_size <- 50
  result <- build_card_geojson(card_src, script_path = NULL, out_path = card_out, cell_size = cell_size)
  expect_equal(result$count, 3)

  geojson <- jsonlite::read_json(card_out)
  ring <- geojson$features[[1]]$geometry$coordinates[[1]]
  lon0 <- ring[[1]][[1]]; lon1 <- ring[[2]][[1]]
  lat0 <- ring[[1]][[2]]; lat1 <- ring[[3]][[2]]
  mlon <- 111320 * cos(lat0 * pi / 180)
  width_m <- abs(lon1 - lon0) * mlon
  height_m <- abs(lat1 - lat0) * 111320
  expect_equal(width_m, cell_size, tolerance = 0.1)
  expect_equal(height_m, cell_size, tolerance = 0.1)

  amounts <- vapply(geojson$features, function(f) {
    a <- f$properties$amount
    if (is.null(a)) 0 else a
  }, numeric(1))
  expect_equal(sum(amounts), 180)
})

test_that("build_card_geojson's cell_size matches build_floating_geojson's when given the same value", {
  float_src <- tempfile(fileext = ".txt")
  float_out <- tempfile(fileext = ".geojson")
  card_src <- tempfile(fileext = ".txt")
  card_out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(float_src, float_out, sub("\\.geojson$", ".qml", float_out), card_src, card_out)))

  writeLines('[[37.5000, 127.0000, 10], [37.5010, 127.0010, 20]]', float_src, useBytes = TRUE)
  float_result <- build_floating_geojson(float_src, script_path = NULL, out_path = float_out, cell_size = 50)
  expect_equal(float_result$cell_size, 50)

  writeLines('{"data": [{"geohash": "wydm9", "amount": 1}]}', card_src, useBytes = TRUE)
  card_result <- build_card_geojson(card_src, script_path = NULL, out_path = card_out,
                                     cell_size = float_result$cell_size)
  expect_equal(card_result$count, 1)
})

test_that("build_floating_geojson converts [lat, lng, count] points into a Polygon grid FeatureCollection", {
  src <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".geojson")
  on.exit(unlink(c(src, out, sub("\\.geojson$", ".qml", out))))

  writeLines('[[37.5, 127.0, 42]]', src, useBytes = TRUE)

  result <- build_floating_geojson(src, script_path = NULL, out_path = out)
  expect_equal(result$count, 1)

  geojson <- jsonlite::read_json(out)
  expect_length(geojson$features, 1)
  feature <- geojson$features[[1]]
  expect_equal(feature$geometry$type, "Polygon")
  expect_equal(feature$properties$value, 42)
  expect_equal(feature$properties$count, 1)

  qml_path <- sub("\\.geojson$", ".qml", out)
  expect_true(file.exists(qml_path))
})

test_that("build_value_grid aggregates multiple points into one cell and fills empty cells with 0", {
  lat <- c(37.5000, 37.5001)
  lng <- c(127.0000, 127.0001)
  value <- c(10, 20)

  grid <- build_value_grid(lat, lng, value, cell_size = 500)
  expect_equal(grid$ncols * grid$nrows, length(grid$features))

  values <- vapply(grid$features, function(f) f$properties$value, numeric(1))
  expect_equal(sum(values), 30)
  expect_true(any(values == 0))
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
