test_that("removed weighted n2 instructions direct users to n1", {
  for (direction in c("h", "v")) {
    for (stat in c("n2", "n2(d=2)", " n2 (d=0) ")) {
      s <- small_plan_session(direction)
      s$out_glob$tab$stat_type <- stat
      expect_error(tabulate_mics_table(s),
                   "Excel row 9, column 3.*n2 was removed.*Use n1",
                   info = paste(direction, stat))
    }
  }
  s <- small_plan_session("v")
  s$out_glob$tab$stat_type <- "n2"
  expect_error(tabulate_extra_table(s, data.frame(label = "Total", value = 6)),
               "n2 was removed.*Use n1")
})

test_that("n1 retains weighted counts and n_unw1 remains supported", {
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    s$out_glob$tab$stat_type <- "n1"
    result <- tabulate_mics_table(s)
    expect_equal(result$value, 6)
    expect_identical(result$value_f_view, "6")
    s$out_glob$tab$stat_type <- "n1(d=2)"
    result <- tabulate_mics_table(s)
    expect_equal(result$value, 6)
    expect_identical(result$value_f_view, "6.00")
  }
  s <- small_plan_session("v")
  s$out_glob$tab$stat_type <- "n_unw1"
  expect_equal(tabulate_mics_table(s)$value, 3)
  s <- small_plan_session("h")
  s$out_glob$tab$stat_type <- "n1"
  result <- tabulate_extra_table(s, data.frame(label = "Total", value = 1234.6))
  expect_equal(result$value, 1234.6)
  expect_identical(result$value_f_view, "1,235")
})

test_that("n1 stays outside count-total consistency checks", {
  s <- small_plan_session("v")
  results <- check_example()
  results$stat_type <- "n"
  results$value[1] <- 4
  expect_identical(row_group_total_check(s, results)$status, "fail")
  results$stat_type <- "n1"
  invisible(capture.output(check <- row_group_total_check(s, results)))
  expect_identical(check$status, "not_applicable")
})

test_that("n1 Excel exports use count formatting and honor explicit precision", {
  s <- small_plan_session("v")
  s$out_glob$condition_row_index <- 4L
  for (stat in c("n1", "n1(d=2)")) {
    s$out_glob$tab$stat_type <- stat
    result <- tabulate_mics_table(s)
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
    write_mics_table(s, path, table = result, formatted = TRUE)
    cells <- tidyxl::xlsx_cells(path, sheets = "Example")
    cell <- cells[cells$address == "C9", ]
    formats <- tidyxl::xlsx_formats(path)$local$numFmt
    expect_equal(cell$numeric, 6)
    expect_equal(formats[cell$local_format_id],
                 if (stat == "n1") "#,##0" else "#,##0.00;(#,##0.00)")
  }
})
