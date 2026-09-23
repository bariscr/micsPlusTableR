test_that("authored footnotes close the table without conditional footnotes", {
  for (merged in c(FALSE, TRUE)) {
    for (note_count in 0:2) {
      path <- tempfile(fileext = ".xlsx")
      on.exit(unlink(path), add = TRUE)
      cells <- matrix(NA_character_, 18L + note_count, 9)
      cells[1, 1] <- "Table 1.3 Number of call attempts"
      cells[3, c(1, 3:7, 9)] <- c("Rows", rep("Total", 5), "IDX")
      cells[4, 2] <- "hh.sav\nfilter(total == 1)\nweight by w"
      cells[4, 3:7] <- "total == 1"
      cells[7, 1:2] <- c("Total", "total == 1")
      cells[7, 3:7] <- "n"
      cells[18, 1:2] <- c("Other", "sex == 2")
      cells[18, 3:7] <- "n"
      notes <- c("A Not called in the current wave", "B Additional explanation")[seq_len(note_count)]
      if (note_count > 0) cells[18L + seq_len(note_count), 1] <- notes
      wb <- openxlsx2::wb_workbook()$add_worksheet("Example")$add_worksheet("IDX")
      wb$add_data("Example", cells, col_names = FALSE)
      wb$add_data("IDX", cells[1, 1], col_names = FALSE)
      wb$add_cell_style("Example", "A18", indent = 1)
      # The template only closes the data body, as in the reported table.
      wb$add_border("Example", "A18:G18", bottom_border = "thin",
                    top_border = NULL, left_border = NULL, right_border = NULL)
      if (merged && note_count > 0) {
        for (row in 18L + seq_len(note_count)) {
          wb$merge_cells("Example", dims = sprintf("A%d:G%d", row, row))
        }
      }
      wb$save(path)
      s <- small_plan_session()
      read_mics_tabulation(s, path, "Example")
      result <- tabulate_mics_table(s)
      expect_equal(max(result$row_index), 18)
      expect_false(any(result$value_f %in% c("-", "(*)") | grepl("^\\(", result$value_f)))

      for (iteration in 1:2) {
        write_mics_footnotes(s, path, table = result)
        written <- tidyxl::xlsx_cells(path, sheets = "Example")
        borders <- tidyxl::xlsx_formats(path)$local$border
        styles <- function(addresses, side) {
          borders[[side]]$style[written$local_format_id[match(addresses, written$address)]]
        }
        last_row <- 18L + note_count
        expect_identical(styles(paste0(LETTERS[1:7], last_row + 1L), "top"), rep("thin", 7))
        expect_identical(styles(paste0("H", 7:last_row), "left"), rep("thin", last_row - 6L))
        expect_identical(styles(paste0(LETTERS[1:7], 18), "bottom"), rep("thin", 7))
        expect_identical(written$character[written$col == 1 & written$row > 18 &
                                            !is.na(written$character)], notes)
        if (merged && note_count > 0) {
          expect_length(openxlsx2::wb_load(path)$worksheets[[1]]$mergeCells, note_count)
        }
      }
    }
  }
})

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
