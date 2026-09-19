expense_filter_session <- function(add_g_filter = FALSE) {
  s <- small_plan_session("h")
  s$hh <- data.frame(total = 1, HCS7 = 1, HCS8 = rep(c(100, 200), 55),
                     group = rep(1:3, c(60, 30, 20)), w = 1)
  s$out_glob$is_supp <- TRUE
  s$out_glob$tab <- tidyr::expand_grid(row_index = 9:12, col_index = 3:8) |>
    dplyr::mutate(stat_type = rep(c("n", "p", "p", "mean(HCS8)", "n", "n_unw"), 4))
  s$out_glob$tab_r <- tibble::tibble(row_index = 9:12, row_lgc = paste("group ==", 1:4))
  s$out_glob$row_header <- tibble::tibble(row_index = 9:12, row_header = paste("Group", 1:4))
  s$out_glob$indent_rows <- tibble::tibble(row = 9:12, indent = 1L)
  s$out_glob$group_info <- tibble::tibble(row = 9:12, grp = "group")
  s$out_glob$tab_c <- tibble::tibble(col_index = 3:8, col_lgc = "total == 1")
  s$out_glob$col_header <- tibble::tibble(col_index = 3:8, col_header = LETTERS[3:8])
  s$out_glob$filter_row <- tibble::tibble(
    row_index = 4L, col_index = c(2L, 6L), df = "hh",
    filter_condition = c("filter(total == 1)", "filter(HCS7 == 1 & HCS8 < 9999998)"),
    calculation = NA_character_, weight = "w")
  if (add_g_filter) {
    s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row,
      dplyr::mutate(s$out_glob$filter_row[2, ], col_index = 7L,
                    filter_condition = "filter(HCS7 == 1)"))
  }
  s
}

test_that("adding a filter in G preserves F's mean and suppression basis", {
  before <- tabulate_h(expense_filter_session())
  after <- tabulate_h(expense_filter_session(TRUE))
  f_before <- before[before$col_index == 6L, ]
  f_after <- after[after$col_index == 6L, ]
  expect_equal(f_after$value, c(150, 150, 150, NaN))
  expect_equal(f_after$n_unw, c(60, 30, 20, 0))
  expect_equal(f_after[c("value", "n_unw", "value_f", "value_f_view")],
               f_before[c("value", "n_unw", "value_f", "value_f_view")])
  expect_equal(f_after$value_f_view, c("150", "(150)", "(*)", "-"))
  expect_equal(after$value[after$col_index == 7L], c(60, 30, 20, 0))
  expect_equal(nrow(after), nrow(before))
})

test_that("F retains its valid-expense restriction independently of G", {
  s <- expense_filter_session(TRUE)
  s$hh$HCS8[1] <- 9999998
  s$hh$HCS7[2] <- 0
  result <- tabulate_h(s)
  f <- result[result$row_index == 9L & result$col_index == 6L, ]
  expect_equal(f$value, 150)
  expect_equal(f$n_unw, 59) # G's filter now also applies to its right-hand n_unw.
  expect_identical(f$value_f_view, "150")
  expect_equal(result$value[result$row_index == 9L & result$col_index == 7L], 59)
})

test_that("the formatted Excel export retains the mean and small-sample markers", {
  s <- expense_filter_session(TRUE)
  s$out_glob$condition_row_index <- 4L
  result <- tabulate_h(s)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  openxlsx2::wb_workbook()$add_worksheet("Example")$save(path)
  write_mics_table(s, path, table = result, formatted = TRUE)
  cells <- tidyxl::xlsx_cells(path, sheets = "Example")
  f <- cells[cells$col == 6L & cells$row %in% 9:12, ]
  expect_equal(f$numeric[1:2], c(150, 150))
  expect_equal(f$character[3:4], c("(*)", "-"))
})

test_that("supplied extra-table values use the same repaired display basis", {
  s <- expense_filter_session(TRUE)
  supplied <- data.frame(label = paste("Group", 1:4), c = c(60, 30, 20, 0),
    d = 100, e = 100, f = c(150, 150, 150, NaN),
    g = c(60, 30, 20, 0), h = c(60, 30, 20, 0))
  result <- tabulate_extra_table(s, supplied)
  f <- result[result$col_index == 6L, ]
  expect_equal(f$n_unw, c(60, 30, 20, 0))
  expect_equal(f$value_f_view, c("150", "(150)", "(*)", "-"))
  expect_equal(nrow(result), 24L)
})

test_that("count recovery respects rows, count groups, and existing matches", {
  repair <- mics_session()$restore_suppression_basis
  cells <- tibble::tibble(
    row_index = c(rep(9L, 7), 10L), col_index = c(3:9, 6L),
    stat_type = c("p", "n_unw", "mean(x)", "mean(x)", "n_unw", "p", "p", "p"),
    value = c(10, 20, 150, 150, 60, 50, 50, 50),
    n_unw = c(NA, 20, 0, NA, 60, NA, NA, NA),
    col_start_1 = c(NA, 3, 5, NA, 7, NA, 9, NA),
    col_end_1 = c(NA, 4, 5, NA, 7, NA, 9, NA), df = "hh")
  out <- repair(cells)
  expect_equal(out$n_unw, c(20, 20, 0, 60, 60, NA, NA, NA))
  expect_equal(out$col_start_1[4], 5)
  expect_equal(out$col_end_1[4], 7)
  expect_equal(nrow(out), nrow(cells))
  cells$df[7 == cells$col_index] <- "hl"
  expect_true(is.na(repair(cells)$n_unw[4]))
})
