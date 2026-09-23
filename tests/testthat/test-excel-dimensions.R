test_that("formatted exports persist exact spacer dimensions over template settings", {
  for (hidden in c(FALSE, TRUE)) {
    s <- small_plan_session()
    cells <- tibble::tibble(row_index = 9L, col_index = 3L, stat_type = "mean",
                            value = NaN, value_f = "-")
    s$out_glob$tab <- s$out_glob$tab_org <- cells
    s$out_glob$condition_row_index <- 4L
    s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    wb <- openxlsx2::wb_workbook()$add_worksheet("Example")
    wb$set_col_widths("Example", cols = 1:3, widths = 12, hidden = hidden)
    wb$set_row_heights("Example", rows = 3:5, heights = 1, hidden = hidden)
    wb$add_data("Example", "condition", dims = "C4")
    wb$save(path)
    before <- openxlsx2::wb_load(path)$worksheets[[1]]
    before_cols <- before$unfold_cols()
    before_rows <- before$sheet_data$row_attr

    for (iteration in 1:2) {
      write_mics_table(s, path, table = cells, formatted = TRUE)
      write_mics_footnotes(s, path, table = cells)
      sheet <- openxlsx2::wb_load(path)$worksheets[[1]]
      columns <- sheet$unfold_cols()
      spacer <- columns[columns$min == "2", ]
      # Reference: Excel's Width dialog set to 0.5 saves OOXML width="1".
      expect_identical(spacer$width, "1")
      expect_true(spacer$hidden %in% c("", "0", "false"))
      expect_identical(spacer$bestFit, "")
      expect_equal(columns[columns$min %in% c("1", "3"), names(before_cols)],
                   before_cols[before_cols$min %in% c("1", "3"), ],
                   ignore_attr = TRUE)

      rows <- sheet$sheet_data$row_attr
      condition <- rows[rows$r == "4", ]
      expect_identical(condition$ht, "3")
      expect_true(condition$hidden %in% c("", "0", "false"))
      expect_identical(condition$customHeight, "1")
      for (row in c("3", "5")) {
        expect_identical(rows$ht[rows$r == row], before_rows$ht[before_rows$r == row])
        expect_identical(rows$hidden[rows$r == row], before_rows$hidden[before_rows$r == row])
      }
    }
  }
})
