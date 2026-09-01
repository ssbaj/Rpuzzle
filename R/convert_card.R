# 카드매출.txt (+ 실시간 재수집 스크립트) -> 카드매출.geojson 변환.

.extract_card_records <- function(data) {
  if (.is_json_array(data)) return(data)
  if (.is_json_object(data) && .is_json_array(data[["data"]])) return(data[["data"]])
  stop("카드매출 데이터에서 geohash 레코드 목록을 찾을 수 없습니다.")
}

#' 카드매출 데이터를 GeoJSON(geohash 격자 Polygon FeatureCollection)으로 저장한다
#'
#' 1) 먼저 \code{script_path}(요청 스크립트, R 코드)를 실행해 실시간 데이터를
#' 시도한다. 2) 실패하면 원본 \code{source_path}(캐시된 JSON)를 사용한다.
#' 자세한 내용은 \code{\link{run_fetch_script}}를 참고한다.
#'
#' @param source_path 카드매출.txt (JSON) 파일 경로.
#' @param script_path 실시간 재수집용 R 스크립트 경로. \code{NULL}이면 건너뛴다.
#' @param out_path 출력 GeoJSON 경로. \code{NULL}이면 \code{source_path}와
#'   같은 폴더에 "카드매출.geojson"으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @return \code{list(out_path = ..., count = ...)}.
#' @export
build_card_geojson <- function(source_path, script_path = NULL, out_path = NULL, log = NULL) {
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

  features <- list()
  for (rec in records) {
    geohash <- rec[["geohash"]]
    if (is.null(geohash) || !nzchar(geohash)) next

    coords <- tryCatch(bbox_polygon_coords(geohash), error = function(e) NULL)
    if (is.null(coords)) next

    properties <- rec[setdiff(names(rec), "geohash")]
    properties[["geohash"]] <- geohash

    features[[length(features) + 1L]] <- list(
      type = "Feature",
      geometry = list(type = "Polygon", coordinates = coords),
      properties = properties
    )
  }

  geojson <- list(type = "FeatureCollection", features = features)

  if (is.null(out_path)) {
    dir <- dirname(source_path)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, "카드매출.geojson")
  }

  jsonlite::write_json(geojson, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE)

  list(out_path = out_path, count = length(features))
}
