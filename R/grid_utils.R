# 위경도 점 데이터(+값) -> 정사각형 격자 GeoJSON(Polygon) 변환 공용 유틸리티.
#
# 유동인구.txt 같은 [[lat, lng, value], ...] 형태의 점 데이터를, 지도 위에서
# 색깔로 바로 구분되도록 정사각형 격자(Polygon)로 집계한다. 격자 한 칸의 크기는
# 점들 사이의 실제 간격(최근접 거리)을 표본으로 재서 자동으로 추정하며, 값 범위에
# 맞춘 5단계 분위수 그라데이션 QML 스타일 파일을 함께 만들어 QGIS에서 열자마자
# 자동으로 색이 입혀지게 한다. 파이썬 pypuzzle의 grid_utils.py를 그대로 이식했다.

.mlat_per_deg <- 111320.0

# 노랑 -> 빨강 그라데이션 (값이 낮음 -> 높음), "r,g,b,a" 문자열
.grid_ramp_colors <- c(
  "255,255,178,255",
  "254,204,92,255",
  "253,141,60,255",
  "240,59,32,255",
  "189,0,38,255"
)
.grid_nodata_color <- "220,220,220,120"

.meters_per_lon <- function(lat_deg) .mlat_per_deg * cos(lat_deg * pi / 180)

#' 점들의 최근접 거리 중앙값으로 격자 한 칸의 크기(m)를 추정한다
#'
#' 표본이 너무 적거나 추정이 실패하면 \code{default}(m)를 그대로 사용한다.
#'
#' 전체 점에 대해 결정론적으로 계산한다(무작위 표본을 쓰지 않는다) — 같은
#' 입력이면 언어/실행 환경에 관계없이(파이썬 pypuzzle 포함) 항상 같은 값이
#' 나오게 하기 위함이다. 중앙값도 통계적 평균이 아니라 파이썬의
#' \code{sorted(dists)[len(dists) // 2]}와 똑같은 규칙(정렬 후 floor(n/2)번째,
#' 0-based)으로 골라 두 언어의 결과를 정확히 일치시킨다.
#'
#' @param lat,lng,value 같은 길이의 numeric 벡터.
#' @param default 추정 실패 시 사용할 기본 셀 크기(m).
#' @return 추정된 셀 크기(m), numeric 스칼라.
#' @export
estimate_cell_size <- function(lat, lng, value, default = 50.0) {
  n <- length(lat)
  if (n < 20) return(default)

  lat0 <- mean(lat)
  mlon <- .meters_per_lon(lat0)

  bucket_size <- max(default * 2, 100.0)
  x <- lng * mlon
  y <- lat * .mlat_per_deg
  bx <- floor(x / bucket_size)
  by <- floor(y / bucket_size)
  bucket_key <- paste(bx, by, sep = "_")

  buckets <- split(seq_len(n), bucket_key)

  dists <- vector("numeric", 0)
  for (i in seq_len(n)) {
    kx <- bx[i]; ky <- by[i]
    best <- Inf
    for (dx in -1:1) {
      for (dy in -1:1) {
        key <- paste(kx + dx, ky + dy, sep = "_")
        idxs <- buckets[[key]]
        if (is.null(idxs)) next
        for (j in idxs) {
          if (j == i) next
          d2 <- (x[j] - x[i])^2 + (y[j] - y[i])^2
          if (d2 < best) best <- d2
        }
      }
    }
    if (is.finite(best)) dists <- c(dists, sqrt(best))
  }

  if (length(dists) == 0) return(default)

  dists <- sort(dists)
  # 파이썬의 dists[len(dists) // 2] (0-based)와 동일 -> 1-based로는 (n \%/\% 2) + 1
  median_dist <- dists[(length(dists) %/% 2L) + 1L]
  if (median_dist < 5 || median_dist > 2000) return(default)
  median_dist
}

# 점들의 bounding box를 cell_size(m) 정사각형 격자로 나누고, 각 점이 속하는
# 칸의 0-based row-major id(cell_id)를 계산한다. build_value_grid와
# build_property_grid가 공유하는 격자 정의 로직이다 — 같은 lat/lng bounding box에
# 같은 cell_size를 지정하면 항상 같은 격자(칸 크기/개수/원점)가 나온다.
.grid_geometry <- function(lat, lng, cell_size) {
  lat0 <- mean(lat)
  mlon <- .meters_per_lon(lat0)

  x <- lng * mlon
  y <- lat * .mlat_per_deg
  x0 <- min(x); y0 <- min(y)
  x1 <- max(x); y1 <- max(y)

  ncols <- as.integer(ceiling((x1 - x0) / cell_size)) + 1L
  nrows <- as.integer(ceiling((y1 - y0) / cell_size)) + 1L

  cx <- as.integer(floor((x - x0) / cell_size))
  cy <- as.integer(floor((y - y0) / cell_size))
  cell_id <- cy * as.numeric(ncols) + cx

  list(mlon = mlon, x0 = x0, y0 = y0, ncols = ncols, nrows = nrows, cell_id = cell_id)
}

# .grid_geometry()가 계산한 원점(x0, y0)/투영(mlon) 기준으로, (col, row) 칸의
# GeoJSON Polygon coordinates(반시계, 첫 점=끝 점)를 만든다.
.grid_cell_polygon <- function(x0, y0, mlon, cell_size, col, row) {
  to_lonlat <- function(gx, gy) c(gx / mlon, gy / .mlat_per_deg)

  gx0 <- x0 + col * cell_size
  gy0 <- y0 + row * cell_size
  gx1 <- gx0 + cell_size
  gy1 <- gy0 + cell_size

  list(list(
    to_lonlat(gx0, gy0),
    to_lonlat(gx1, gy0),
    to_lonlat(gx1, gy1),
    to_lonlat(gx0, gy1),
    to_lonlat(gx0, gy0)
  ))
}

#' \code{[lat, lng, value]} 점들을 정사각형 격자 Polygon Feature 목록으로 변환한다
#'
#' 점들의 bounding box 전체를 \code{cell_size}(m) 크기의 격자로 채우며(빈 칸은
#' value=0), 각 칸의 속성으로 "value"(칸에 속한 점들의 값 합)와 "count"(칸에
#' 속한 점 개수)를 담는다.
#'
#' @param lat,lng,value 같은 길이의 numeric 벡터.
#' @param cell_size 격자 한 칸의 크기(m). \code{NULL}이면
#'   \code{\link{estimate_cell_size}}로 자동 추정한다.
#' @return \code{list(features = ..., cell_size = ..., ncols = ..., nrows = ...)}.
#' @export
build_value_grid <- function(lat, lng, value, cell_size = NULL) {
  n <- length(lat)
  if (n == 0) {
    return(list(features = list(), cell_size = if (is.null(cell_size)) 50.0 else cell_size,
                ncols = 0L, nrows = 0L))
  }

  if (is.null(cell_size)) {
    cell_size <- estimate_cell_size(lat, lng, value)
  }

  geo <- .grid_geometry(lat, lng, cell_size)
  ncols <- geo$ncols; nrows <- geo$nrows; cell_id <- geo$cell_id
  mlon <- geo$mlon; x0 <- geo$x0; y0 <- geo$y0

  # cell_id (0-based) -> 1-based sequential index in row-major (row*ncols+col)
  # order, matching exactly the order features are built in below, so the
  # aggregated sums/counts can be looked up by plain integer indexing instead
  # of error-prone name-based lookup.
  total_cells <- as.numeric(ncols) * as.numeric(nrows)
  value_full <- numeric(total_cells)
  count_full <- integer(total_cells)

  agg_val <- tapply(value, cell_id, sum)
  agg_cnt <- tapply(value, cell_id, length)
  idx <- as.integer(names(agg_val)) + 1L
  value_full[idx] <- as.numeric(agg_val)
  count_full[idx] <- as.integer(agg_cnt)

  features <- vector("list", ncols * nrows)
  k <- 0L
  for (row in 0:(nrows - 1L)) {
    for (col in 0:(ncols - 1L)) {
      k <- k + 1L
      features[[k]] <- list(
        type = "Feature",
        geometry = list(type = "Polygon",
                         coordinates = .grid_cell_polygon(x0, y0, mlon, cell_size, col, row)),
        properties = list(value = unname(value_full[k]), count = as.integer(count_full[k]))
      )
    }
  }

  list(features = features, cell_size = cell_size, ncols = ncols, nrows = nrows)
}

#' 위경도 점들을 정사각형 격자로 묶어, 여러 속성(수치/문자)을 함께 집계한다
#'
#' \code{\link{build_value_grid}}의 일반화 버전이다. 각 점마다 수치 속성
#' 여러 개(칸 안에서 합산)와 문자 속성 여러 개(칸 안에서 관측된 유일값을
#' ","로 이어붙임)를 함께 붙일 수 있다.
#'
#' 격자 정의(칸 크기, 원점, 칸 개수)는 \code{cell_size}와 점들의 bounding box
#' 만으로 결정된다. 즉 다른 데이터셋(예: 카드매출 geohash 중심점)에도 같은
#' \code{cell_size}를 지정하면, 서로 다른 원본 데이터로 만든 두 GeoJSON의
#' 격자 칸 크기를 동일하게 맞출 수 있다.
#'
#' @param lat,lng 같은 길이의 numeric 벡터. 각 점의 위경도.
#' @param cell_size 격자 한 칸의 크기(m). 다른 데이터셋과 격자 크기를 맞추려면
#'   그 데이터셋에 쓴 것과 같은 값을 지정한다.
#' @param numeric_props 이름이 붙은 numeric 벡터들의 list(각 벡터 길이는
#'   \code{lat}과 같아야 한다). 칸 안에서 합산되어 같은 이름의 속성으로 담긴다.
#' @param text_props 이름이 붙은 character 벡터들의 list(각 벡터 길이는
#'   \code{lat}과 같아야 한다). 칸 안에서 관측된 유일값을 정렬 후 ","로
#'   이어붙여 같은 이름의 속성으로 담는다.
#' @return \code{list(features = ..., cell_size = ..., ncols = ..., nrows = ...)}.
#'   각 feature의 속성에는 \code{numeric_props}/\code{text_props}로 준 이름들과
#'   더불어, 칸에 집계된 원본 점 개수를 담은 "count"가 포함된다.
#' @export
build_property_grid <- function(lat, lng, cell_size, numeric_props = list(), text_props = list()) {
  n <- length(lat)
  if (n == 0) {
    return(list(features = list(), cell_size = cell_size, ncols = 0L, nrows = 0L))
  }

  geo <- .grid_geometry(lat, lng, cell_size)
  ncols <- geo$ncols; nrows <- geo$nrows; cell_id <- geo$cell_id
  mlon <- geo$mlon; x0 <- geo$x0; y0 <- geo$y0

  total_cells <- as.numeric(ncols) * as.numeric(nrows)

  count_full <- integer(total_cells)
  agg_cnt <- tapply(cell_id, cell_id, length)
  count_full[as.integer(names(agg_cnt)) + 1L] <- as.integer(agg_cnt)

  numeric_full <- lapply(numeric_props, function(v) {
    full <- numeric(total_cells)
    agg <- tapply(v, cell_id, sum, na.rm = TRUE)
    full[as.integer(names(agg)) + 1L] <- as.numeric(agg)
    full
  })

  text_full <- lapply(text_props, function(v) {
    full <- character(total_cells)
    agg <- tapply(v, cell_id, function(vals) paste(sort(unique(vals[nzchar(vals)])), collapse = ","))
    full[as.integer(names(agg)) + 1L] <- as.character(agg)
    full
  })

  features <- vector("list", ncols * nrows)
  k <- 0L
  for (row in 0:(nrows - 1L)) {
    for (col in 0:(ncols - 1L)) {
      k <- k + 1L
      properties <- list()
      for (name in names(numeric_full)) properties[[name]] <- unname(numeric_full[[name]][k])
      for (name in names(text_full)) properties[[name]] <- text_full[[name]][k]
      properties[["count"]] <- as.integer(count_full[k])

      features[[k]] <- list(
        type = "Feature",
        geometry = list(type = "Polygon",
                         coordinates = .grid_cell_polygon(x0, y0, mlon, cell_size, col, row)),
        properties = properties
      )
    }
  }

  list(features = features, cell_size = cell_size, ncols = ncols, nrows = nrows)
}

.grid_quantile_breaks <- function(values, n = 5) {
  values <- sort(values)
  total <- length(values)
  q <- function(p) values[min(total, max(1L, floor(p * total) + 1L))]
  breaks <- vapply(seq_len(n - 1L), function(i) q(i / n), numeric(1))
  c(breaks, values[total])
}

#' "value" 속성 기준 5단계 분위수 그라데이션 QML 스타일을 저장한다
#'
#' 같은 이름(\verb{<geojson 파일명>.qml})으로 저장해두면 QGIS가 레이어를 열 때
#' 자동으로 이 스타일을 적용해, 별도 설정 없이 격자가 색깔로 표시된다. 0(빈 칸)은
#' 옅은 회색 반투명으로 별도 처리한다.
#'
#' @param qml_path 저장할 .qml 경로.
#' @param attr 그라데이션을 적용할 속성 이름.
#' @param values 격자 셀 값들의 numeric 벡터. \code{breaks}가 없으면 이것으로
#'   분위수 구간을 계산한다.
#' @param breaks 구간 상한값 5개(오름차순). \code{NULL}이면 \code{values}의
#'   0 초과 값으로 분위수를 계산한다.
#' @return 사용된 \code{breaks} (invisible).
#' @export
write_graduated_qml <- function(qml_path, attr = "value", values = NULL, breaks = NULL) {
  if (is.null(breaks)) {
    nonzero <- values[values > 0]
    if (length(nonzero) == 0) {
      breaks <- c(1, 2, 3, 4, 5)
    } else {
      breaks <- .grid_quantile_breaks(nonzero, n = length(.grid_ramp_colors))
    }
  }

  ranges_xml <- c(paste0(
    '      <range lower="0.000000000000000" upper="0.000000000000000" ',
    'symbol="0" label="0 (no data)" render="true"/>'
  ))
  lower_prev <- 0
  for (i in seq_along(breaks)) {
    upper <- breaks[i]
    label_lower <- if (i > 1) lower_prev + 1 else lower_prev
    ranges_xml <- c(ranges_xml, sprintf(
      '      <range lower="%s.000000000000000" upper="%s.000000000000000" symbol="%d" label="%s - %s" render="true"/>',
      format(lower_prev, scientific = FALSE), format(upper, scientific = FALSE), i,
      format(label_lower, scientific = FALSE), format(upper, scientific = FALSE)
    ))
    lower_prev <- upper
  }

  symbol_block <- function(name, color) {
    sprintf(paste0(
      '      <symbol type="fill" name="%s" alpha="1" clip_to_extent="1" force_rhr="0">\n',
      '        <layer pass="0" class="SimpleFill" locked="0" enabled="1">\n',
      '          <prop k="color" v="%s"/>\n',
      '          <prop k="outline_color" v="%s"/>\n',
      '          <prop k="outline_style" v="solid"/>\n',
      '          <prop k="outline_width" v="0.1"/>\n',
      '          <prop k="style" v="solid"/>\n',
      '        </layer>\n',
      '      </symbol>'
    ), name, color, if (identical(name, "0")) "150,150,150,255" else "80,80,80,255")
  }

  symbols_xml <- c(symbol_block("0", .grid_nodata_color))
  for (i in seq_along(.grid_ramp_colors)) {
    symbols_xml <- c(symbols_xml, symbol_block(as.character(i), .grid_ramp_colors[i]))
  }

  qml <- sprintf(paste0(
    "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>\n",
    '<qgis version="3.28" styleCategories="AllStyleCategories">\n',
    '  <renderer-v2 attr="%s" type="graduatedSymbol" graduatedMethod="GraduatedColor" forceraster="0" symbollevels="0" enableorderby="0">\n',
    "    <ranges>\n%s\n    </ranges>\n",
    "    <symbols>\n%s\n    </symbols>\n",
    "  </renderer-v2>\n",
    "  <blendMode>0</blendMode>\n",
    "  <layerOpacity>1</layerOpacity>\n",
    "</qgis>\n"
  ), attr, paste(ranges_xml, collapse = "\n"), paste(symbols_xml, collapse = "\n"))

  writeLines(qml, qml_path, useBytes = TRUE)

  invisible(breaks)
}
