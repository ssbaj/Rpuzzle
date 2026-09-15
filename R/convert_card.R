# 카드매출.txt (+ 실시간 재수집 스크립트) -> 카드매출.geojson 변환.
#
# 원본 데이터는 geohash 문자열로 격자(cell)를 표현하는데, geohash 칸 크기는
# 문자열 길이(정밀도)로 고정되어 있어 유동인구.txt를 격자화할 때 쓰는 cell_size(m)와
# 보통 일치하지 않는다(예: geohash 7자리는 약 120m x 150m, 유동인구는 보통 50m).
# 그래서 각 geohash는 그 중심점(위경도)으로만 취급하고, build_property_grid()로
# 유동인구와 동일한 cell_size의 정사각형 격자에 다시 담아(속성은 칸 안에서 합산)
# 두 GeoJSON의 격자 크기가 항상 일치하도록 한다.

.extract_card_records <- function(data) {
  if (.is_json_array(data)) return(data)
  if (.is_json_object(data) && .is_json_array(data[["data"]])) return(data[["data"]])
  stop("카드매출 데이터에서 geohash 레코드 목록을 찾을 수 없습니다.")
}

# geohash 레코드 목록을, 중심 위경도(lat/lng)와 그 외 속성(수치는 numeric_props,
# 문자는 text_props)으로 분해한다. geohash 자체도 text_props$geohash로 보존해,
# 격자화 후에도 어떤 geohash들이 한 칸에 모였는지 알 수 있게 한다.
.card_records_to_points <- function(records) {
  valid <- list()
  lat <- numeric(0); lng <- numeric(0)
  for (rec in records) {
    geohash <- rec[["geohash"]]
    if (is.null(geohash) || !nzchar(geohash)) next
    center <- tryCatch(decode_center(geohash), error = function(e) NULL)
    if (is.null(center)) next

    valid[[length(valid) + 1L]] <- rec
    lat[[length(valid)]] <- center[["lat"]]
    lng[[length(valid)]] <- center[["lon"]]
  }

  n <- length(valid)
  prop_names <- unique(unlist(lapply(valid, function(rec) setdiff(names(rec), "geohash"))))

  is_numeric_prop <- vapply(prop_names, function(name) {
    for (rec in valid) {
      val <- rec[[name]]
      if (!is.null(val) && length(val) == 1L) return(is.numeric(val))
    }
    TRUE
  }, logical(1))

  numeric_props <- list()
  for (name in prop_names[is_numeric_prop]) numeric_props[[name]] <- rep(NA_real_, n)
  text_props <- list(geohash = character(n))
  for (name in prop_names[!is_numeric_prop]) text_props[[name]] <- rep("", n)

  for (i in seq_len(n)) {
    rec <- valid[[i]]
    text_props[["geohash"]][i] <- rec[["geohash"]]
    for (name in prop_names) {
      val <- rec[[name]]
      if (is.null(val) || length(val) != 1L) next
      if (name %in% names(numeric_props)) {
        numeric_props[[name]][i] <- as.numeric(val)
      } else {
        text_props[[name]][i] <- as.character(val)
      }
    }
  }

  list(lat = lat, lng = lng, numeric_props = numeric_props, text_props = text_props, n = n)
}

#' 카드매출 데이터를 GeoJSON(정사각형 격자 Polygon FeatureCollection)으로 저장한다
#'
#' 1) 먼저 \code{script_path}(요청 스크립트, R 코드)를 실행해 실시간 데이터를
#' 시도한다. 2) 실패하면 원본 \code{source_path}(캐시된 JSON)를 사용한다.
#' 자세한 내용은 \code{\link{run_fetch_script}}를 참고한다.
#'
#' 각 geohash 레코드는 중심점(위경도)으로 변환한 뒤 \code{cell_size}(m) 크기의
#' 정사각형 격자에 다시 집계한다(수치 속성은 칸 안에서 합산). \code{cell_size}에
#' \code{\link{build_floating_geojson}}가 만든 유동인구 격자와 같은 값을 지정하면
#' 두 GeoJSON의 격자 칸 크기가 정확히 일치한다(기본값 50m은 유동인구 격자의
#' 통상적인 자동 추정값과 같다). 유동인구 격자 크기가 다르게 추정됐다면, 그
#' 결과의 \code{cell_size}(예: \code{build_floating_geojson(...)$cell_size})를
#' 그대로 넘겨 맞추는 것이 정확하다.
#'
#' @param source_path 카드매출.txt (JSON) 파일 경로.
#' @param script_path 실시간 재수집용 R 스크립트 경로. \code{NULL}이면 건너뛴다.
#' @param out_path 출력 GeoJSON 경로. \code{NULL}이면 \code{source_path}와
#'   같은 폴더에 "카드매출_YYMMDDHHMMSS.geojson"(생성 시각)으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @param cell_size 격자 한 칸의 크기(m). 유동인구 격자와 맞추려면 그 격자의
#'   \code{cell_size}를 그대로 지정한다. 기본값은 50m.
#' @return \code{list(out_path = ..., count = ...)}. \code{count}는 격자에
#'   집계된 원본 geohash 레코드 수다.
#' @export
build_card_geojson <- function(source_path, script_path = NULL, out_path = NULL, log = NULL,
                                cell_size = 50.0) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  fetch <- run_fetch_script(script_path)
  if (!is.null(fetch$data)) {
    data <- fetch$data
    log("실시간 재수집 성공 (카드매출 재수집 스크립트 실행)")
  } else {
    log(sprintf("실시간 재수집 실패 -> 저장된 파일 사용. 사유: %s", fetch$fail_reason))
    data <- load_json_flexible(source_path)
  }

  records <- .extract_card_records(data)
  points <- .card_records_to_points(records)

  grid <- build_property_grid(points$lat, points$lng, cell_size = cell_size,
                               numeric_props = points$numeric_props,
                               text_props = points$text_props)
  log(sprintf("격자 크기 %.0fm x %.0fm (유동인구와 동일 기준), %d x %d = %d칸, 원본 %d건 집계",
              grid$cell_size, grid$cell_size, grid$ncols, grid$nrows, length(grid$features), points$n))

  geojson <- list(type = "FeatureCollection", features = grid$features)

  if (is.null(out_path)) {
    dir <- dirname(source_path)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, sprintf("카드매출_%s.geojson", .output_timestamp()))
  }

  jsonlite::write_json(geojson, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = 15)

  list(out_path = out_path, count = points$n)
}
