#' Rpuzzle 'JSON 합치기' GUI를 실행한다
#'
#' 유동인구/카드매출/상권 JSON 파일을 각각 2개씩 지정해 하나로 합칠 수 있는
#' 팝업창을 띄운다. \code{\link{puzzle_app2}}를 호출한다.
#'
#' @return (보이지 않게) \code{\link{puzzle_app2}}의 반환값.
#' @export
run2 <- function() {
  invisible(puzzle_app2())
}
