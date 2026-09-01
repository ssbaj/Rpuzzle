#' Rpuzzle GUI를 실행한다
#'
#' 패키지의 진입점. \code{\link{puzzle_app}}을 호출해 GUI 팝업창을 띄운다.
#'
#' @return (보이지 않게) \code{\link{puzzle_app}}의 반환값.
#' @export
run <- function() {
  invisible(puzzle_app())
}
