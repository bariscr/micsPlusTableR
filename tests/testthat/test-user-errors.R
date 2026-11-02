test_that("cell errors locate the expression and statistic", {
  expect_error(cell_case(row = "missing_variable == 1"),
    "Excel row 9.*row condition: missing_variable == 1")
  expect_error(cell_case(col = "missing_variable == 1"),
    "Excel row 9, column 3.*column condition: missing_variable == 1")
  expect_error(cell_case(col = "sex =="), "column condition: sex ==")
  expect_error(cell_case("unknown_stat"), "Unsupported statistic 'unknown_stat'")
  expect_error(cell_case(weighted = "yes"), "'weighted' must be")
  expect_error(cell_case(df = data.frame(sex = 1)), "Weight column 'w'.*not found")
  for (weight in list("one", -1, Inf)) {
    expect_error(cell_case(df = data.frame(sex = 1, w = weight)),
      "Weight column 'w'.*numeric, non-negative")
  }
})

test_that("direction errors name worksheet and failing block instead of only an index", {
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    calculate <- getExportedValue("micsPlusTableR", paste0("tabulate_", direction))
    s$out_glob$filter_row$filter_condition <- "filter(missing_variable == 1)"
    message <- tryCatch(calculate(s), error = conditionMessage)
    expect_match(message, "Worksheet 'Example'", fixed = TRUE)
    expect_match(message, "missing_variable", fixed = TRUE)
    expect_false(grepl("In index:", message, fixed = TRUE))
    if (direction == "h") expect_match(message, "horizontal block 1")
    s$out_glob$filter_row$filter_condition <- "filter(total == 1)"
    s$out_glob$filter_row$df <- "absent"
    expect_error(calculate(s), "Prepared data source 'absent'.*prepare_mics_data")
    s$out_glob$filter_row$df <- "hh"
    s$out_glob$tab$stat_type <- "unknown_stat"
    expect_error(calculate(s), "Unsupported statistic")
    s$out_glob$tab$stat_type <- "n"
    s$out_glob$tab_c$col_lgc <- "bad_variable == 1"
    expect_error(calculate(s), "column.*3.*bad_variable")
  }
})

test_that("public helpers validate arguments before calling the engine", {
  expect_error(mics_session(hh = 1), "'hh' must be a data frame")
  expect_error(tabulate_v(mics_session()), "read_mics_tabulation")
  expect_error(tabulate_v(small_plan_session("h")), "uses direction 'h'")
  expect_error(tabulate_h(small_plan_session(), NA), "skip_row_conditions")
  expect_error(row_condition_f(data.frame(x = 1)), "missing required columns: row_index, row_lgc")
  expect_error(col_condition_f(NULL), "'tab_c' must be a data frame")
  expect_error(filter_second(data.frame(x = 1)), "at least two columns")
  expect_error(extract_var(c("x", "y")), "one condition string")
  expect_error(get_blank_cols(data.frame(row_index = 1)), "needs worksheet columns")
  expect_error(check_mics_table(mics_session(), data.frame(x = 1)), "missing required columns")
  for (bad in list(NA, Inf, -1, c(1, 2), "one")) {
    expect_error(check_mics_table(mics_session(), check_example(), tolerance = bad), "finite, non-negative")
    expect_error(row_group_total_check(mics_session(), check_example(), diff = bad), "finite, non-negative")
  }
  expect_error(pivot_mics_table(mics_session(), data.frame(x = 1), formatted = NA), "formatted")
})

test_that("comparison errors explain shapes, options, and thresholds", {
  expect_error(compare_mics_tables(data.frame(x = 1), data.frame(x = 1:2)),
    "Row counts differ: previous table has 1; current table has 2")
  expect_error(compare_mics_tables(data.frame(x = 1), data.frame(x = 1, y = 2)),
    "Column counts differ: previous table has 1; current table has 2")
  expect_error(compare_mics_tables(NULL, data.frame(x = 1)), "must be a data frame")
  expect_error(compare_mics_tables(data.frame(x = 1), data.frame(x = 1), show_diff_over = Inf),
    "show_diff_over.*finite, non-negative")
  expect_error(compare_mics_tables(data.frame(x = 1), data.frame(x = 1), ignore_col_names = NA),
    "ignore_col_names.*TRUE or FALSE")
})

test_that("workbook errors identify paths, sheets, and malformed plans", {
  for (path in list(NULL, NA_character_, character(), c("a", "b"))) {
    expect_error(read_mics_tabulation(mics_session(), path, "Example"), "one non-empty file path")
  }
  path <- tempfile(fileext = ".xlsx")
  openxlsx2::write_xlsx(list(Example = data.frame(a = 1:8, b = "text", c = "text")), path)
  for (sheet in list("Missing", 0, 1.5, 99, NA, character())) {
    expect_error(read_mics_tabulation(mics_session(), path, sheet), "Available sheets: Example")
  }
  expect_error(read_mics_tabulation(mics_session(), path, "Example"), "B4:B7")
  expect_error(read_previous_mics_table(path, "Example", skip = -1), "non-negative whole number")
})

test_that("missing worksheet statistics explain which cells need calculation instructions", {
  path <- tempfile(fileext = ".xlsx")
  cells <- matrix(NA_character_, 8, 7)
  cells[1, 1] <- "Table 2.1a Sources of electricity"
  cells[1, 7] <- "IDX"
  cells[3, 3] <- "Total"
  cells[4, 2] <- "hh.sav\nfilter(total == 1)\nweight by w"
  cells[4, 3] <- "total == 1"
  cells[7:8, 1] <- c("Electricity", "Number of households")
  cells[7:8, 2] <- "total == 1"
  write_plan <- function() {
    wb <- openxlsx2::wb_workbook()$add_worksheet("2.1a")$add_worksheet("IDX")
    wb$add_data("2.1a", cells, col_names = FALSE)
    wb$add_cell_style("2.1a", dims = "A7:A8", horizontal = "left", indent = 1)
    wb$add_data("IDX", "Table 2.1a Sources of electricity", col_names = FALSE)
    wb$save(path, overwrite = TRUE)
  }
  for (blank in c(NA_character_, "   ")) {
    cells[7:8, 3] <- blank
    write_plan()
    error <- tryCatch(read_mics_tabulation(mics_session(), path, "2.1a"), error = identity)
    expect_s3_class(error, "error")
    message <- micsPlusTableR:::mics_error_display(error)$summary
    expect_match(message, "Worksheet '2.1a' has no statistic types defined", fixed = TRUE)
    expect_match(message, "'p' (percentage) or 'n' (count)", fixed = TRUE)
    expect_match(message, "column C onward, below the data-source/filter row", fixed = TRUE)
  }
  cells[7:8, 3] <- c("p", "n")
  write_plan()
  s <- mics_session(hh = data.frame(total = 1, w = c(1, 2)))
  expect_no_error(read_mics_tabulation(s, path, "2.1a"))
  expect_equal(tabulate_mics_table(s)$value, c(100, 3))
})

test_that("missing statistics in a supplied plan stay distinct from missing conditions", {
  for (direction in c("h", "v")) {
    s <- small_plan_session(direction)
    for (blank in list(s$out_glob$tab[0, ], NA_character_, "   ")) {
      if (is.data.frame(blank)) s$out_glob$tab <- blank else {
        s$out_glob$tab <- small_plan_session(direction)$out_glob$tab
        s$out_glob$tab$stat_type <- blank
      }
      expect_error(tabulate_mics_table(s), "Worksheet 'Example' has no statistic types defined")
    }
    s <- small_plan_session(direction)
    s$out_glob$tab_r <- s$out_glob$tab_r[0, ]
    expect_error(tabulate_mics_table(s), "no usable 'tab_r' entries")
  }
})

test_that("preparation errors identify the script and underlying cause", {
  hh <- tempfile(fileext = ".sav"); hl <- tempfile(fileext = ".sav")
  file.create(hh, hl)
  prep <- tempfile(fileext = ".R")
  writeLines("stop('The survey variable age is missing')", prep)
  plan <- preparation_plan(prep)
  message <- tryCatch(prepare_mics_data(mics_session(), hh, hl, plan), error = conditionMessage)
  expect_match(message, basename(plan), fixed = TRUE)
  expect_match(message, "Preparation File", fixed = TRUE)
  expect_match(message, "survey variable age is missing", fixed = TRUE)
})

test_that("output helpers reject invalid paths and flags with named arguments", {
  s <- small_plan_session()
  s$cell_results <- check_example()
  expect_error(write_mics_table(s, NA_character_), "destination.*one non-empty file path")
  expect_error(write_mics_footnotes(s, character()), "destination.*one non-empty file path")
  path <- tempfile(fileext = ".xlsx")
  openxlsx2::write_xlsx(list(Example = data.frame(a = 1)), path)
  expect_error(write_mics_table(s, path, formatted = NA), "formatted.*TRUE or FALSE")
  expect_error(write_mics_table(s, path, drop_n_unw = "yes"), "drop_n_unw.*TRUE or FALSE")
  expect_error(write_mics_footnotes(s, path, sheet = "Missing"), "Available sheets: Example")
})
