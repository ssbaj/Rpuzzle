# 외부 라이브러리 없이 동작하는 표준 geohash 디코더.
#
# 퍼즐(puzzle.geovision.co.kr) 카드매출 API는 위경도 대신 geohash 문자열로
# 격자(cell)를 표현한다. 이 모듈은 geohash 문자열을 위경도 사각형(bbox)으로
# 복원해, 격자를 지도 위 폴리곤으로 그릴 수 있게 해준다.

.base32 <- "0123456789bcdefghjkmnpqrstuvwxyz"
.decode_map <- seq_along(strsplit(.base32, "")[[1]]) - 1L
names(.decode_map) <- strsplit(.base32, "")[[1]]

#' geohash 문자열을 위경도 사각형(bbox)으로 디코딩한다
#'
#' @param geohash geohash 문자열.
#' @return c(lat_min, lat_max, lon_min, lon_max) 이름이 붙은 numeric 벡터.
#' @export
decode_bbox <- function(geohash) {
  lat_range <- c(-90, 90)
  lon_range <- c(-180, 180)
  is_lon <- TRUE

  chars <- strsplit(tolower(trimws(geohash)), "")[[1]]
  for (ch in chars) {
    if (!ch %in% names(.decode_map)) {
      stop(sprintf("'%s'는 올바른 geohash 문자가 아닙니다.", ch))
    }
    idx <- .decode_map[[ch]]
    for (bit in 4:0) {
      bitval <- bitwAnd(bitwShiftR(idx, bit), 1L)
      if (is_lon) {
        mid <- mean(lon_range)
        if (bitval == 1L) lon_range[1] <- mid else lon_range[2] <- mid
      } else {
        mid <- mean(lat_range)
        if (bitval == 1L) lat_range[1] <- mid else lat_range[2] <- mid
      }
      is_lon <- !is_lon
    }
  }

  c(lat_min = lat_range[1], lat_max = lat_range[2],
    lon_min = lon_range[1], lon_max = lon_range[2])
}

#' geohash 문자열을 중심 좌표로 디코딩한다
#'
#' @param geohash geohash 문자열.
#' @return c(lat, lon) 이름이 붙은 numeric 벡터.
#' @export
decode_center <- function(geohash) {
  bbox <- decode_bbox(geohash)
  c(lat = mean(bbox[c("lat_min", "lat_max")]),
    lon = mean(bbox[c("lon_min", "lon_max")]))
}

#' geohash 셀 경계를 GeoJSON Polygon 좌표로 변환한다
#'
#' 반시계 방향, 첫 점과 끝 점이 같은 좌표 리스트를 반환한다.
#'
#' @param geohash geohash 문자열.
#' @return GeoJSON Polygon의 \code{coordinates} 필드에 바로 넣을 수 있는
#'   중첩 리스트 (1개의 링, 5개의 [lon, lat] 점).
#' @export
bbox_polygon_coords <- function(geohash) {
  bbox <- decode_bbox(geohash)
  lat_min <- bbox[["lat_min"]]; lat_max <- bbox[["lat_max"]]
  lon_min <- bbox[["lon_min"]]; lon_max <- bbox[["lon_max"]]

  list(list(
    c(lon_min, lat_min),
    c(lon_max, lat_min),
    c(lon_max, lat_max),
    c(lon_min, lat_max),
    c(lon_min, lat_min)
  ))
}
