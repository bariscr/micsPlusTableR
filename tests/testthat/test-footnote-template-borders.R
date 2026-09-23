test_that("footnotes included in a parsed plan lose their closing border before notes are appended", {
  for (merged in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    cells <- matrix(NA_character_, 21, 15)
    cells[1, 1] <- "Table 1.2 Average number of call attempts"
    cells[3, c(1, 3:13, 15)] <- c("Rows", rep("Total", 11), "IDX")
    cells[4, 2] <- "hh.sav\nfilter(total == 1)\nweight by w"
    cells[4, 3:13] <- "total == 1"
    cells[7, 1:2] <- c("Total", "total == 1")
    cells[7, 3:13] <- "mean(sex)"
    cells[20, 1:2] <- c("Other", "sex == 99")
    cells[20, 3:13] <- "mean(sex)"
    cells[21, 1] <- "A It does not include households with the following response codes"
    wb <- openxlsx2::wb_workbook()$add_worksheet("Example")$add_worksheet("IDX")
    wb$add_data("Example", cells, col_names = FALSE)
    wb$add_data("IDX", "Table 1.2 Average number of call attempts", col_names = FALSE)
    wb$add_cell_style("Example", "A20", indent = 1)
    if (merged) wb$merge_cells("Example", dims = "A21:M21")
    wb$add_border("Example", "A21:M21", top_border = "thin", bottom_border = "double",
                  left_border = "thin", right_border = "thin")
    wb$add_border("Example", "A22:M22", top_border = "thin", bottom_border = NULL,
                  left_border = NULL, right_border = NULL)
    wb$save(path)

    s <- small_plan_session()
    plan <- read_mics_tabulation(s, path, "Example")
    result <- tabulate_mics_table(s)
    # The parsed plan extends into the note, but the calculated data ends at row 20.
    expect_equal(max(plan$tab$row_index), 21)
    expect_equal(max(result$row_index), 20)
    # Expand and shrink the generated block to exercise old closing borders too.
    for (note_count in c(1L, 3L, 1L)) {
      s$out_glob$is_supp <- note_count == 3L
      if (note_count == 3L) result$value_f[1:2] <- c("(1.8)", "(*)")
      write_mics_footnotes(s, path, table = result)
      written <- tidyxl::xlsx_cells(path, sheets = "Example")
      borders <- tidyxl::xlsx_formats(path)$local$border
      styles <- function(row, side) {
        borders[[side]]$style[written$local_format_id[match(paste0(LETTERS[1:13], row), written$address)]]
      }
      expect_true(all(is.na(styles(21, "bottom")) | styles(21, "bottom") == "none"))
      expect_true(all(is.na(styles(22, "top")) | styles(22, "top") == "none"))
      expect_identical(styles(21, "top"), rep("thin", 13))
      expect_identical(styles(22 + note_count, "top"), rep("thin", 13))
      for (row in seq.int(22L, 21L + note_count)) {
        expect_true(all(is.na(styles(row, "bottom")) | styles(row, "bottom") == "none"))
        expect_true(all(is.na(styles(row, "top")) | styles(row, "top") == "none"))
      }
      if (note_count == 1L) {
        expect_true(all(is.na(styles(25, "top")) | styles(25, "top") == "none"))
      }
      expect_identical(written$character[written$address == "A21"], cells[21, 1])
      expect_identical(written$character[written$address == paste0("A", 21 + note_count)],
                       "- denotes 0 unweighted cases in the denominator")
      if (merged) expect_true(any(grepl("A21:M21", openxlsx2::wb_load(path)$worksheets[[1]]$mergeCells)))
    }
  }
})
