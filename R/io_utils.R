# 파일 로딩 및 'p파일(요청 스크립트)' 재실행을 담당하는 공용 유틸리티.

.encodings <- c("UTF-8", "CP949", "EUC-KR")

.strip_utf8_bom <- function(raw) {
  bom <- as.raw(c(0xEF, 0xBB, 0xBF))
  if (length(raw) >= 3 && identical(raw[1:3], bom)) raw[-(1:3)] else raw
}

#' 인코딩을 몰라도(UTF-8 / UTF-8 BOM / CP949 / EUC-KR) JSON 파일을 읽어들인다
#'
#' @param path JSON 파일 경로.
#' @return \code{jsonlite::fromJSON(..., simplifyVector = FALSE)}로 파싱된
#'   중첩 list.
#' @export
load_json_flexible <- function(path) {
  raw <- .strip_utf8_bom(readBin(path, "raw", n = file.info(path)$size))

  last_err <- NULL
  for (enc in .encodings) {
    txt <- tryCatch({
      if (identical(enc, "UTF-8")) rawToChar(raw) else iconv(rawToChar(raw), from = enc, to = "UTF-8")
    }, error = function(e) e)
    if (inherits(txt, "error")) {
      last_err <- txt
      next
    }

    result <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                        error = function(e) e)
    if (!inherits(result, "error")) return(result)
    last_err <- result
  }

  stop(sprintf("'%s' 파일을 알려진 인코딩(%s)으로 읽을 수 없습니다: %s",
               path, paste(.encodings, collapse = ", "), conditionMessage(last_err)))
}

#' 'p유동인구.txt' / 'p카드매출.txt' 같은 요청 스크립트를 실행해 실시간 데이터를 재수집한다
#'
#' 파이썬 버전은 \code{requests.get(...)} 호출로 끝나는 파이썬 스크립트를
#' \code{exec()}했지만, R 버전에서는 스크립트가 R 코드여야 하며 마지막에
#' \code{httr::GET(...)} 등으로 만든 \code{response} 객체를 환경에 남겨야 한다.
#'
#' 쿠키 만료/네트워크 오류 등 어떤 이유로든 실패하면 \code{data = NULL}과
#' 실패 사유 문자열을 반환하고, 호출 측이 저장된 원본 파일로 대체(fallback)할
#' 수 있게 한다.
#'
#' @param script_path 실행할 R 스크립트 경로. \code{NULL}이거나 빈 문자열이면
#'   즉시 실패로 취급한다.
#' @param timeout 스크립트 실행 중 적용할 네트워크 타임아웃(초).
#' @return \code{list(data = ..., fail_reason = ...)}. 성공 시 \code{data}에
#'   파싱된 JSON, 실패 시 \code{data = NULL}과 \code{fail_reason} 문자열.
#' @export
run_fetch_script <- function(script_path, timeout = 6) {
  if (is.null(script_path) || !nzchar(script_path)) {
    return(list(data = NULL, fail_reason = "수집 스크립트가 지정되지 않았습니다."))
  }
  if (!file.exists(script_path)) {
    return(list(data = NULL,
                fail_reason = sprintf("스크립트 파일을 열 수 없습니다: '%s' 파일이 없습니다.", script_path)))
  }

  source_text <- tryCatch(rawToChar(.strip_utf8_bom(readBin(script_path, "raw",
                                                             n = file.info(script_path)$size))),
                           error = function(e) e)
  if (inherits(source_text, "error")) {
    return(list(data = NULL,
                fail_reason = sprintf("스크립트 파일을 열 수 없습니다: %s", conditionMessage(source_text))))
  }

  env <- new.env(parent = globalenv())
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)

  exec_err <- tryCatch({
    eval(parse(text = source_text), envir = env)
    NULL
  }, error = function(e) e)
  if (!is.null(exec_err)) {
    return(list(data = NULL, fail_reason = sprintf("스크립트 실행 중 오류: %s", conditionMessage(exec_err))))
  }

  response <- env[["response"]]
  if (is.null(response)) {
    return(list(data = NULL, fail_reason = "스크립트에 'response' 변수가 없습니다."))
  }

  status <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
  if (is.na(status) || status != 200L) {
    return(list(data = NULL,
                fail_reason = sprintf("서버 응답 실패 (status=%s). 쿠키/세션이 만료되었을 수 있습니다.",
                                      if (is.na(status)) "NA" else status)))
  }

  parsed <- tryCatch(httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8"),
                      error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(data = NULL,
                fail_reason = sprintf("응답을 JSON으로 해석할 수 없습니다: %s", conditionMessage(parsed))))
  }

  list(data = parsed, fail_reason = NULL)
}
