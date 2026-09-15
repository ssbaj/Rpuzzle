# 프로그램 실행 시 뜨는 메인 팝업창.
# 유동인구/카드매출/상권 원본 파일(txt)을 지정하고 버튼을 클릭하면
# GeoJSON을 생성한다.

.autodetect_bases <- c("C:/dataPy", "c:/dataPy")

.set_var_if_exists <- function(var, path) {
  if (file.exists(path) && !nzchar(tcltk::tclvalue(var))) {
    tcltk::tclvalue(var) <- path
  }
}

.notify <- function(title, message, icon = "info") {
  tcltk::tkmessageBox(title = title, message = message, icon = icon, type = "ok")
}

#' Rpuzzle GUI 팝업창을 띄운다
#'
#' 유동인구/카드매출/상권 원본 데이터(txt)를 지정하고 버튼을 클릭하면
#' 해당 GeoJSON 파일을 생성하는 Tcl/Tk 팝업창을 띄운다. 파이썬 pypuzzle의
#' \code{MainApp} (tkinter)을 그대로 이식한 것이다.
#'
#' @return (보이지 않게) 생성된 최상위 Tcl/Tk 창 핸들.
#' @export
puzzle_app <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("이 GUI 기능을 사용하려면 'tcltk' 패키지가 필요합니다.", call. = FALSE)
  }

  win <- tcltk::tktoplevel()
  tcltk::tkwm.title(win, "Rpuzzle - 퍼즐 상권데이터 GeoJSON 변환기")
  tcltk::tkwm.geometry(win, "640x430")
  tcltk::tkwm.resizable(win, 0, 0)

  floating_var <- tcltk::tclVar("")
  card_var <- tcltk::tclVar("")
  area_var <- tcltk::tclVar("")

  frame <- tcltk::ttkframe(win, padding = 20)
  tcltk::tkpack(frame, fill = "both", expand = TRUE)

  title_font <- tryCatch(tcltk::tkfont.create(size = 15, weight = "bold"), error = function(e) NULL)
  title_lbl <- if (is.null(title_font)) {
    tcltk::ttklabel(frame, text = "Rpuzzle")
  } else {
    tcltk::ttklabel(frame, text = "Rpuzzle", font = title_font)
  }
  tcltk::tkpack(title_lbl, anchor = "w")

  tcltk::tkpack(
    tcltk::ttklabel(
      frame,
      text = "퍼즐(puzzle.geovision.co.kr) 상권 데이터를 GeoJSON으로 변환합니다.",
      foreground = "#555555"
    ),
    anchor = "w", pady = c(0, 15)
  )

  browse_file <- function(var) {
    path <- as.character(tcltk::tkgetOpenFile(
      filetypes = "{{Text files} {.txt}} {{All files} *}"
    ))
    if (nzchar(path)) tcltk::tclvalue(var) <- path
  }

  add_file_row <- function(parent, label_text, var) {
    row <- tcltk::ttkframe(parent)
    tcltk::tkpack(row, fill = "x", pady = 3)
    tcltk::tkpack(tcltk::ttklabel(row, text = label_text, width = 32, anchor = "w"), side = "left")
    tcltk::tkpack(tcltk::ttkentry(row, textvariable = var), side = "left", fill = "x",
                  expand = TRUE, padx = c(0, 6))
    tcltk::tkpack(tcltk::ttkbutton(row, text = "찾아보기", command = function() browse_file(var)),
                  side = "left")
  }

  add_action_button <- function(parent, label_text, command) {
    tcltk::tkpack(tcltk::ttkbutton(parent, text = label_text, command = command),
                  fill = "x", pady = c(4, 16), ipady = 8)
  }

  generate_floating <- function() {
    data_path <- trimws(tcltk::tclvalue(floating_var))
    if (!nzchar(data_path) || !file.exists(data_path)) {
      .notify("입력 확인", "유동인구.txt 파일을 지정해주세요.", icon = "warning")
      return(invisible(NULL))
    }
    result <- tryCatch(build_floating_geojson(data_path, NULL), error = function(e) e)
    if (inherits(result, "error")) {
      .notify("오류", sprintf("유동인구 GeoJSON 생성 중 오류가 발생했습니다:\n%s", conditionMessage(result)),
              icon = "error")
      return(invisible(NULL))
    }
    .notify("완료", sprintf("유동인구 GeoJSON 생성 완료\n\n%s\n지점 수: %d", result$out_path, result$count))
  }

  generate_card <- function() {
    data_path <- trimws(tcltk::tclvalue(card_var))
    if (!nzchar(data_path) || !file.exists(data_path)) {
      .notify("입력 확인", "카드매출.txt 파일을 지정해주세요.", icon = "warning")
      return(invisible(NULL))
    }
    result <- tryCatch(build_card_geojson(data_path, NULL), error = function(e) e)
    if (inherits(result, "error")) {
      .notify("오류", sprintf("카드매출 GeoJSON 생성 중 오류가 발생했습니다:\n%s", conditionMessage(result)),
              icon = "error")
      return(invisible(NULL))
    }
    .notify("완료", sprintf("카드매출 GeoJSON 생성 완료\n\n%s\n격자 수: %d", result$out_path, result$count))
  }

  generate_area <- function() {
    data_path <- trimws(tcltk::tclvalue(area_var))
    if (!nzchar(data_path) || !file.exists(data_path)) {
      .notify("입력 확인", "상권.txt 파일을 지정해주세요.", icon = "warning")
      return(invisible(NULL))
    }
    result <- tryCatch(build_area_geojson(data_path), error = function(e) e)
    if (inherits(result, "error")) {
      .notify("오류", sprintf("상권 GeoJSON 생성 중 오류가 발생했습니다:\n%s", conditionMessage(result)),
              icon = "error")
      return(invisible(NULL))
    }
    .notify("완료", sprintf("상권 GeoJSON 생성 완료\n\n%s\n상권 수: %d", result$out_path, result$count))
  }

  add_file_row(frame, "유동인구 데이터 (유동인구.txt)", floating_var)
  add_action_button(frame, "\U0001F6B6 유동인구 GeoJSON 생성", generate_floating)

  tcltk::tkpack(tcltk::ttkseparator(frame), fill = "x", pady = c(0, 16))

  add_file_row(frame, "카드매출 데이터 (카드매출.txt)", card_var)
  add_action_button(frame, "\U0001F4B3 카드매출 GeoJSON 생성", generate_card)

  tcltk::tkpack(tcltk::ttkseparator(frame), fill = "x", pady = c(0, 16))

  add_file_row(frame, "상권 데이터 (상권.txt)", area_var)
  add_action_button(frame, "\U0001F3E2 상권 GeoJSON 생성", generate_area)

  tcltk::tkpack(tcltk::ttkbutton(frame, text = "닫기", command = function() tcltk::tkdestroy(win)),
                fill = "x", pady = c(10, 0), ipady = 6)

  for (base in .autodetect_bases) {
    if (dir.exists(base)) {
      .set_var_if_exists(floating_var, file.path(base, "유동인구.txt"))
      .set_var_if_exists(card_var, file.path(base, "카드매출.txt"))
      .set_var_if_exists(area_var, file.path(base, "상권.txt"))
      break
    }
  }

  tcltk::tkwait.window(win)
  invisible(win)
}
