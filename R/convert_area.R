# 상권.txt -> 상권.geojson 변환.

.is_json_array <- function(x) is.list(x) && is.null(names(x))
.is_json_object <- function(x) is.list(x) && !is.null(names(x))

.extract_areas <- function(data) {
  if (.is_json_array(data)) return(data)
  if (.is_json_object(data) && .is_json_array(data[["contents"]])) return(data[["contents"]])
  stop("상권 데이터에서 'contents' 목록을 찾을 수 없습니다.")
}

.areas_to_features <- function(areas) {
  features <- list()
  for (area in areas) {
    detail <- area[["areaMetaDetail"]]
    if (is.null(detail)) detail <- list()
    geometry <- detail[["geometry"]]
    if (is.null(geometry)) next

    rltm <- area[["rltm"]]
    if (is.null(rltm)) rltm <- list()

    properties <- list(
      areaId = area[["areaId"]],
      areaName = area[["areaName"]],
      congestion = rltm[["congestion"]],
      congestionLevel = rltm[["congestionLevel"]],
      datetime = rltm[["datetime"]],
      repLat = detail[["repLat"]],
      repLng = detail[["repLng"]],
      areaM2 = detail[["areaM2"]]
    )

    features[[length(features) + 1L]] <- list(
      type = "Feature",
      geometry = geometry,
      properties = properties
    )
  }
  features
}

#' 상권 데이터를 GeoJSON(Polygon FeatureCollection)으로 저장한다
#'
#' @param source_path 상권.txt (JSON) 파일 경로.
#' @param out_path 출력 GeoJSON 경로. \code{NULL}이면 \code{source_path}와
#'   같은 폴더에 "상권.geojson"으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @return \code{list(out_path = ..., count = ...)}.
#' @export
build_area_geojson <- function(source_path, out_path = NULL, log = NULL) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  data <- load_json_flexible(source_path)
  areas <- .extract_areas(data)
  features <- .areas_to_features(areas)

  geojson <- list(type = "FeatureCollection", features = features)

  if (is.null(out_path)) {
    dir <- dirname(source_path)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, "상권.geojson")
  }

  jsonlite::write_json(geojson, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE)

  log(sprintf("상권 GeoJSON 생성 완료: %s (%d개 상권)", out_path, length(features)))
  list(out_path = out_path, count = length(features))
}
