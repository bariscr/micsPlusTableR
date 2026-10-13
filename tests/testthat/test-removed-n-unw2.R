test_that("removed n_unw2 instructions direct users to n_unw1", {
  for (direction in c("h", "v")) {
    for (stat in c("n_unw2", "n_unw2(d=2)")) {
      s <- small_plan_session(direction)
      s$out_glob$tab$stat_type <- stat
      expect_error(tabulate_mics_table(s),
                   "Excel row 9, column 3.*n_unw2 was removed.*Use n_unw1")
    }
  }
})

test_that("n_unw1 calculates and formats unweighted counts in both directions", {
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    s$out_glob$tab$stat_type <- "n_unw1"
    result <- tabulate_mics_table(s)
    expect_equal(result$value, 3)
    expect_identical(result$value_f_view, "3")
    s$out_glob$tab$stat_type <- "n_unw1(d=2)"
    expect_identical(tabulate_mics_table(s)$value_f_view, "3.00")
  }
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "n_unw1"
  result <- tabulate_extra_table(s, data.frame(label = "Total", value = 1234))
  expect_identical(result$value_f_view, "1,234")
  results <- check_example()
  results$stat_type <- "n_unw1"
  invisible(capture.output(check <- row_group_total_check(s, results)))
  expect_identical(check$status, "not_applicable")
})

test_that("n_unw1 Excel export preserves count formats and optional blanking", {
  s <- small_plan_session("h")
  s$out_glob$condition_row_index <- 4L
  s$out_glob$tab$stat_type <- "n_unw1"
  result <- tabulate_mics_table(s)
  for (drop in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
    write_mics_table(s, path, table = result, formatted = TRUE, drop_n_unw = drop)
    cells <- tidyxl::xlsx_cells(path, sheets = "Example")
    cell <- cells[cells$address == "C9", ]
    if (drop) {
      expect_true(all(is.na(cell$numeric)))
      expect_true(all(is.na(cell$character) | cell$character == ""))
    } else {
      expect_equal(cell$numeric, 3)
      formats <- tidyxl::xlsx_formats(path)$local$numFmt
      expect_equal(formats[cell$local_format_id], "#,##0")
    }
  }
})
