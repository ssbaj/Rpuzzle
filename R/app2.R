# 'JSON 합치기' 팝업창.
# 같은 종류(유동인구/카드매출/상권)의 JSON 파일 2개를 지정하고 버튼을 클릭하면
# 하나로 합쳐진 JSON(_merged.json) 파일을 만든다.

#' Rpuzzle 'JSON 합치기' 팝업창을 띄운다
#'
#' 유동인구/카드매출/상권 JSON 파일을 각각 2개씩 지정하고 버튼을 클릭하면
#' 두 파일의 데이터를 하나로 합친 "_merged.json" 파일을 생성하는
#' Tcl/Tk 팝업창을 띄운다.
#'
#' @return (보이지 않게) 생성된 최상위 Tcl/Tk 창 핸들.
#' @export
puzzle_app2 <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("이 GUI 기능을 사용하려면 'tcltk' 패키지가 필요합니다.", call. = FALSE)
  }

  win <- tcltk::tktoplevel()
  tcltk::tkwm.title(win, "Rpuzzle - JSON 파일 합치기")
  tcltk::tkwm.geometry(win, "640x560")
  tcltk::tkwm.resizable(win, 0, 0)

  floating_var1 <- tcltk::tclVar("")
  floating_var2 <- tcltk::tclVar("")
  card_var1 <- tcltk::tclVar("")
  card_var2 <- tcltk::tclVar("")
  area_var1 <- tcltk::tclVar("")
  area_var2 <- tcltk::tclVar("")

  frame <- tcltk::ttkframe(win, padding = 20)
  tcltk::tkpack(frame, fill = "both", expand = TRUE)

  title_font <- tryCatch(tcltk::tkfont.create(size = 15, weight = "bold"), error = function(e) NULL)
  title_lbl <- if (is.null(title_font)) {
    tcltk::ttklabel(frame, text = "Rpuzzle - JSON 합치기")
  } else {
    tcltk::ttklabel(frame, text = "Rpuzzle - JSON 합치기", font = title_font)
  }
  tcltk::tkpack(title_lbl, anchor = "w")

  tcltk::tkpack(
    tcltk::ttklabel(
      frame,
      text = "같은 종류의 JSON 파일 2개를 지정하면 하나로 합쳐줍니다.",
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
    tcltk::tkpack(tcltk::ttklabel(row, text = label_text, width = 20, anchor = "w"), side = "left")
    tcltk::tkpack(tcltk::ttkentry(row, textvariable = var), side = "left", fill = "x",
                  expand = TRUE, padx = c(0, 6))
    tcltk::tkpack(tcltk::ttkbutton(row, text = "찾아보기", command = function() browse_file(var)),
                  side = "left")
  }

  add_action_button <- function(parent, label_text, command) {
    tcltk::tkpack(tcltk::ttkbutton(parent, text = label_text, command = command),
                  fill = "x", pady = c(4, 16), ipady = 8)
  }

  do_merge <- function(var1, var2, merge_fn, label, unit) {
    path1 <- trimws(tcltk::tclvalue(var1))
    path2 <- trimws(tcltk::tclvalue(var2))

    if (!nzchar(path1) || !file.exists(path1)) {
      .notify("입력 확인", sprintf("%s 파일1을 지정해주세요.", label), icon = "warning")
      return(invisible(NULL))
    }
    if (!nzchar(path2) || !file.exists(path2)) {
      .notify("입력 확인", sprintf("%s 파일2를 지정해주세요.", label), icon = "warning")
      return(invisible(NULL))
    }

    result <- tryCatch(merge_fn(path1, path2), error = function(e) e)
    if (inherits(result, "error")) {
      .notify("오류", sprintf("%s JSON 합치기 중 오류가 발생했습니다:\n%s", label, conditionMessage(result)),
              icon = "error")
      return(invisible(NULL))
    }
    .notify("완료", sprintf("%s JSON 합치기 완료\n\n%s\n%s 수: %d", label, result$out_path, unit, result$count))
  }

  tcltk::tkpack(tcltk::ttklabel(frame, text = "\U0001F6B6 유동인구 JSON 합치기", foreground = "#333333"),
                anchor = "w")
  add_file_row(frame, "파일1", floating_var1)
  add_file_row(frame, "파일2", floating_var2)
  add_action_button(frame, "유동인구 Json파일 합치기",
                     function() do_merge(floating_var1, floating_var2, merge_floating_json, "유동인구", "지점"))

  tcltk::tkpack(tcltk::ttkseparator(frame), fill = "x", pady = c(0, 16))

  tcltk::tkpack(tcltk::ttklabel(frame, text = "\U0001F4B3 카드매출 JSON 합치기", foreground = "#333333"),
                anchor = "w")
  add_file_row(frame, "파일1", card_var1)
  add_file_row(frame, "파일2", card_var2)
  add_action_button(frame, "카드매출 Json파일 합치기",
                     function() do_merge(card_var1, card_var2, merge_card_json, "카드매출", "레코드"))

  tcltk::tkpack(tcltk::ttkseparator(frame), fill = "x", pady = c(0, 16))

  tcltk::tkpack(tcltk::ttklabel(frame, text = "\U0001F3E2 상권 JSON 합치기", foreground = "#333333"),
                anchor = "w")
  add_file_row(frame, "파일1", area_var1)
  add_file_row(frame, "파일2", area_var2)
  add_action_button(frame, "상권 Json파일 합치기",
                     function() do_merge(area_var1, area_var2, merge_area_json, "상권", "상권"))

  tcltk::tkpack(tcltk::ttkbutton(frame, text = "닫기", command = function() tcltk::tkdestroy(win)),
                fill = "x", pady = c(10, 0), ipady = 6)

  tcltk::tkwait.window(win)
  invisible(win)
}
