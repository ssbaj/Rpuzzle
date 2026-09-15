# 유동인구.txt (+ 실시간 재수집 스크립트) -> 유동인구.geojson(격자) 변환.
#
# 점 데이터를 그대로 찍지 않고, 점 간격을 기준으로 추정한 정사각형 격자에
# 값을 집계해 Polygon으로 저장한다. 같은 이름의 .qml 스타일 파일을 함께 만들어
# QGIS에서 열자마자 값 크기에 따라 자동으로 색이 입혀지도록 한다.
# grid_utils.R(파이썬 pypuzzle의 grid_utils.py를 이식한 버전)을 사용한다.

.extract_floating_points <- function(data) {
  if (.is_json_array(data)) return(data)
  if (.is_json_object(data)) {
    if (.is_json_array(data[["data"]])) return(data[["data"]])
    page_props <- data[["pageProps"]]
    if (.is_json_object(page_props)) {
      floating <- page_props[["floatingData"]]
      if (.is_json_object(floating) && .is_json_array(floating[["data"]])) return(floating[["data"]])
    }
  }
  stop("유동인구 데이터에서 'pageProps.floatingData.data' 좌표 목록을 찾을 수 없습니다.")
}

.floating_points_to_lat_lng_value <- function(points) {
  lat <- numeric(0); lng <- numeric(0); value <- numeric(0)
  for (pt in points) {
    if (!is.list(pt) || length(pt) < 3L) next
    lat <- c(lat, pt[[1]])
    lng <- c(lng, pt[[2]])
    value <- c(value, pt[[3]])
  }
  list(lat = lat, lng = lng, value = value)
}

#' 유동인구 데이터를 GeoJSON(정사각형 격자 Polygon FeatureCollection)으로 저장한다
#'
#' 1) 먼저 \code{script_path}(요청 스크립트, R 코드)를 실행해 실시간 데이터를
#' 시도한다. 2) 실패하면 원본 \code{source_path}(캐시된 JSON)를 사용한다.
#' 자세한 내용은 \code{\link{run_fetch_script}}를 참고한다.
#'
#' 점들의 bounding box 전체를 격자(빈 칸 포함)로 채우고, 각 칸에 속한 점들의
#' 값을 합산해 "value" 속성으로 담는다. 격자 한 칸의 크기(m)는 \code{cell_size}로
#' 직접 지정하지 않으면 점 간격을 재서 자동으로 추정한다(퍼즐 API가 항상 같은
#' 줌 레벨로 응답하는 한 보통 50m). 같은 이름의 .qml 스타일 파일도 함께 저장해
#' QGIS에서 열면 값 크기별로 자동 색상이 입혀지게 한다.
#'
#' @param source_path 유동인구.txt (JSON) 파일 경로.
#' @param script_path 실시간 재수집용 R 스크립트 경로. \code{NULL}이면 건너뛴다.
#' @param out_path 출력 GeoJSON 경로. \code{NULL}이면 \code{source_path}와
#'   같은 폴더에 "유동인구_YYMMDDHHMMSS.geojson"(생성 시각)으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @param cell_size 격자 한 칸의 크기(m). \code{NULL}이면 자동으로 추정한다.
#' @return \code{list(out_path = ..., count = ...)}. \code{count}는 격자에
#'   집계된 원본 지점 수다.
#' @export
build_floating_geojson <- function(source_path, script_path = NULL, out_path = NULL, log = NULL,
                                    cell_size = NULL) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  fetch <- run_fetch_script(script_path)
  if (!is.null(fetch$data)) {
    data <- fetch$data
    log("실시간 재수집 성공 (유동인구 재수집 스크립트 실행)")
  } else {
    log(sprintf("실시간 재수집 실패 -> 저장된 파일 사용. 사유: %s", fetch$fail_reason))
    data <- load_json_flexible(source_path)
  }

  points <- .extract_floating_points(data)
  parsed <- .floating_points_to_lat_lng_value(points)

  grid <- build_value_grid(parsed$lat, parsed$lng, parsed$value, cell_size = cell_size)
  log(sprintf("격자 크기 %.0fm x %.0fm, %d x %d = %d칸",
              grid$cell_size, grid$cell_size, grid$ncols, grid$nrows, length(grid$features)))

  geojson <- list(type = "FeatureCollection", features = grid$features)

  if (is.null(out_path)) {
    dir <- dirname(source_path)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, sprintf("유동인구_%s.geojson", .output_timestamp()))
  }

  jsonlite::write_json(geojson, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = 15)

  qml_path <- sub("\\.geojson$", ".qml", out_path)
  values <- vapply(grid$features, function(f) f$properties$value, numeric(1))
  write_graduated_qml(qml_path, values = values)
  log(sprintf("QGIS 스타일 저장: %s", qml_path))

  list(out_path = out_path, count = length(parsed$lat))
}
