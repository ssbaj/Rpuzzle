# 같은 종류(유동인구/카드매출/상권)의 JSON 파일 2개를 하나로 합친다.
# 각 파일의 데이터 목록(포인트/레코드/상권 배열)을 찾아 이어붙인 뒤,
# 첫 번째 파일의 구조(배열 그대로 / data 필드 / pageProps 중첩 / contents 필드)를
# 그대로 유지한 채 합쳐진 목록을 다시 그 자리에 채워 넣고 JSON으로 저장한다.

.rebuild_floating <- function(template, points) {
  if (.is_json_array(template)) return(points)
  if (.is_json_object(template)) {
    if (.is_json_array(template[["data"]])) {
      template[["data"]] <- points
      return(template)
    }
    page_props <- template[["pageProps"]]
    if (.is_json_object(page_props)) {
      floating <- page_props[["floatingData"]]
      if (.is_json_object(floating) && .is_json_array(floating[["data"]])) {
        floating[["data"]] <- points
        page_props[["floatingData"]] <- floating
        template[["pageProps"]] <- page_props
        return(template)
      }
    }
  }
  points
}

.rebuild_card <- function(template, records) {
  if (.is_json_array(template)) return(records)
  if (.is_json_object(template) && .is_json_array(template[["data"]])) {
    template[["data"]] <- records
    return(template)
  }
  records
}

.rebuild_area <- function(template, areas) {
  if (.is_json_array(template)) return(areas)
  if (.is_json_object(template) && .is_json_array(template[["contents"]])) {
    template[["contents"]] <- areas
    return(template)
  }
  areas
}

#' 유동인구 JSON 파일 2개를 하나로 합친다
#'
#' 두 유동인구.txt(JSON) 파일에서 좌표 목록을 각각 추출해 이어붙인 뒤,
#' 첫 번째 파일의 구조를 유지한 채 병합된 JSON을 저장한다.
#'
#' @param path1 첫 번째 유동인구 JSON 파일 경로.
#' @param path2 두 번째 유동인구 JSON 파일 경로.
#' @param out_path 출력 JSON 경로. \code{NULL}이면 \code{path1}과 같은 폴더에
#'   "유동인구_merged.json"으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @return \code{list(out_path = ..., count = ...)}.
#' @export
merge_floating_json <- function(path1, path2, out_path = NULL, log = NULL) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  data1 <- load_json_flexible(path1)
  data2 <- load_json_flexible(path2)

  points1 <- .extract_floating_points(data1)
  points2 <- .extract_floating_points(data2)
  merged_points <- c(points1, points2)

  merged <- .rebuild_floating(data1, merged_points)

  if (is.null(out_path)) {
    dir <- dirname(path1)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, "유동인구_merged.json")
  }

  jsonlite::write_json(merged, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  log(sprintf("유동인구 JSON 합치기 완료: %s (%d개 지점)", out_path, length(merged_points)))

  list(out_path = out_path, count = length(merged_points))
}

#' 카드매출 JSON 파일 2개를 하나로 합친다
#'
#' 두 카드매출.txt(JSON) 파일에서 geohash 레코드 목록을 각각 추출해 이어붙인 뒤,
#' 첫 번째 파일의 구조를 유지한 채 병합된 JSON을 저장한다.
#'
#' @param path1 첫 번째 카드매출 JSON 파일 경로.
#' @param path2 두 번째 카드매출 JSON 파일 경로.
#' @param out_path 출력 JSON 경로. \code{NULL}이면 \code{path1}과 같은 폴더에
#'   "카드매출_merged.json"으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @return \code{list(out_path = ..., count = ...)}.
#' @export
merge_card_json <- function(path1, path2, out_path = NULL, log = NULL) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  data1 <- load_json_flexible(path1)
  data2 <- load_json_flexible(path2)

  records1 <- .extract_card_records(data1)
  records2 <- .extract_card_records(data2)
  merged_records <- c(records1, records2)

  merged <- .rebuild_card(data1, merged_records)

  if (is.null(out_path)) {
    dir <- dirname(path1)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, "카드매출_merged.json")
  }

  jsonlite::write_json(merged, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  log(sprintf("카드매출 JSON 합치기 완료: %s (%d개 레코드)", out_path, length(merged_records)))

  list(out_path = out_path, count = length(merged_records))
}

#' 상권 JSON 파일 2개를 하나로 합친다
#'
#' 두 상권.txt(JSON) 파일에서 상권 목록을 각각 추출해 이어붙인 뒤,
#' 첫 번째 파일의 구조를 유지한 채 병합된 JSON을 저장한다.
#'
#' @param path1 첫 번째 상권 JSON 파일 경로.
#' @param path2 두 번째 상권 JSON 파일 경로.
#' @param out_path 출력 JSON 경로. \code{NULL}이면 \code{path1}과 같은 폴더에
#'   "상권_merged.json"으로 저장한다.
#' @param log 진행 메시지를 받을 1-인자 함수. \code{NULL}이면 무시한다.
#' @return \code{list(out_path = ..., count = ...)}.
#' @export
merge_area_json <- function(path1, path2, out_path = NULL, log = NULL) {
  if (is.null(log)) log <- function(msg) invisible(NULL)

  data1 <- load_json_flexible(path1)
  data2 <- load_json_flexible(path2)

  areas1 <- .extract_areas(data1)
  areas2 <- .extract_areas(data2)
  merged_areas <- c(areas1, areas2)

  merged <- .rebuild_area(data1, merged_areas)

  if (is.null(out_path)) {
    dir <- dirname(path1)
    if (!nzchar(dir)) dir <- "."
    out_path <- file.path(dir, "상권_merged.json")
  }

  jsonlite::write_json(merged, out_path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  log(sprintf("상권 JSON 합치기 완료: %s (%d개 상권)", out_path, length(merged_areas)))

  list(out_path = out_path, count = length(merged_areas))
}
