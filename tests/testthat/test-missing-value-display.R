test_that("missing medians display a dash in both directions and supplied tables", {
  for (direction in c("h", "v")) {
    for (enabled in c(FALSE, TRUE)) {
      s <- small_plan_session(direction)
      s$out_glob$is_supp <- enabled
      s$hh$sex <- NA_real_
      s$out_glob$tab$stat_type <- "median(sex, d=2)"
      results <- list(tabulate_mics_table(s))
      if (direction == "h") {
        results <- c(results, list(tabulate_extra_table(s,
          data.frame(label = "Total", value = NA_real_))))
      }
      for (result in results) {
        expect_true(is.na(result$value))
        expect_false(is.nan(result$value))
        expect_identical(result$value_f, "-")
        expect_identical(result$value_f_view, "-")
      }
    }
  }
  s <- small_plan_session("v")
  s$hh$sex <- NA_real_
  s$out_glob$tab$stat_type <- "median_unw(sex)"
  expect_identical(tabulate_v(s)$value_f, "-")
})

test_that("the NA conversion requires a median statistic", {
  s <- small_plan_session()
  for (stat in c("median(sex)", "mean(sex)", "n", "p")) {
    s$out_glob$tab$stat_type <- stat
    for (value in c(NA_real_, NaN, 3)) {
      result <- tabulate_extra_table(s, data.frame(label = "Total", value = value))
      expect_identical(result$value, value)
      if (is.nan(value) || (stat == "median(sex)" && is.na(value))) {
        expect_identical(result$value_f, "-")
      } else {
        expect_false(identical(result$value_f, "-"))
      }
    }
  }
})

test_that("Excel converts median NA and writes a single dash footnote for overlapping rules", {
  for (enabled in c(FALSE, TRUE)) {
    s <- small_plan_session()
    s$out_glob$is_supp <- enabled
    s$out_glob$condition_row_index <- 4L
    cells <- tibble::tibble(row_index = 9:15, col_index = 3L,
      stat_type = c("median(x)", "median(x, d=2)", "mean(x)", "p", "p", "p", "median(x)"),
      value = c(NA, NA, NA, NaN, NaN, 42, 3),
      value_f = c("-", "-", NA, "-", "-", "(42.0)", "3"))
    s$out_glob$tab <- s$out_glob$tab_org <- cells
    s$out_glob$a_cells <- tibble::tibble(row = 1L, col = 5L, character = "IDX")
    for (formatted in c(FALSE, TRUE)) {
      path <- tempfile(fileext = ".xlsx")
      on.exit(unlink(path), add = TRUE)
      openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
      write_mics_table(s, path, table = cells, formatted = formatted)
      for (iteration in 1:2) {
        write_mics_footnotes(s, path, table = cells)
        written <- tidyxl::xlsx_cells(path, sheets = "Example")
        expect_identical(written$character[match(c("C9", "C10", "C12", "C13"), written$address)],
                         rep("-", 4))
        expect_false(any(written$character[written$address == "C11"] == "-", na.rm = TRUE))
        expect_equal(written$numeric[written$address == "C15"], 3)
        expect_equal(sum(written$character == "- denotes 0 unweighted cases in the denominator",
                         na.rm = TRUE), 1)
      }
    }
  }
})
