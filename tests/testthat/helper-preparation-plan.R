preparation_plan <- function(script = NULL, code = readLines(script, warn = FALSE),
                             split_lines = FALSE, is_included = 1L) {
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("IDX")$add_worksheet("Preparation File")
  wb$add_data("IDX", c("Test", "TST", "2026", "Wave 1"), col_names = FALSE)
  pieces <- if (split_lines) code else paste(code, collapse = "\n")
  wb$add_data("Preparation File", data.frame(code = pieces, is_included = is_included),
              col_names = TRUE)
  wb$save(path)
  path
}
