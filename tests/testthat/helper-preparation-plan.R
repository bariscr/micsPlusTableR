preparation_plan <- function(script = NULL, code = readLines(script, warn = FALSE),
                             split_lines = FALSE) {
  path <- tempfile(fileext = ".xlsx")
  wb <- openxlsx2::wb_workbook()$add_worksheet("IDX")$add_worksheet("Preparation File")
  wb$add_data("IDX", c("Test", "TST", "2026", "Wave 1"), col_names = FALSE)
  wb$add_data("Preparation File", if (split_lines) code else paste(code, collapse = "\n"),
              col_names = FALSE)
  wb$save(path)
  path
}
