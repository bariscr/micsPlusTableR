test_that("smaller helpers are public", {
  exports <- c("tabulate_v", "tabulate_h", "calc_cells", "tabulate_extra_table",
    "row_condition_f", "col_condition_f", "normalize_condition_text", "extract_var",
    "get_blank_cols", "filter_second", vapply(micsPlusTableR:::mics_check_specs(), `[[`, "", 1))
  expect_true(all(exports %in% getNamespaceExports("micsPlusTableR")))
  engine <- mics_session()
  expect_false(any(c("tabulate_v2", "tabulate_h2") %in% ls(engine)))
})

test_that("both direction helpers calculate and store the result in isolation", {
  for (direction in c("v", "h")) {
    session <- small_plan_session(direction)
    calculate <- getExportedValue("micsPlusTableR", paste0("tabulate_", direction))
    result <- calculate(session)
    expect_equal(result$value, 6)
    expect_identical(session$cell_results, result)
    expect_equal(tabulate_mics_table(session)$value, result$value)
    expect_null(mics_session()$cell_results)
    session$out_glob$tab$stat_type <- "n_unw"
    expect_equal(calculate(session)$value, 3)
  }
})

test_that("cell calculations retain weighted and unweighted semantics", {
  expect_equal(cell_case()$value, 5)
  expect_equal(cell_case(weighted = FALSE)$value, 2)
  expect_equal(cell_case(weighted = "both")$value, 2)
  expect_equal(cell_case("p")$value, 100 * 5/6)
  expect_equal(cell_case("n_unw")$value, 2)
  expect_equal(cell_case("mean(sex)")$value, 2)
  expect_equal(cell_case("median(sex)")$value, 2)
  expect_equal(cell_case("100")$value, 100)
  expect_true(is.na(cell_case(NA_character_)$value))
  expect_equal(cell_case(row = "ph")$row_logic, "ph")
  expect_equal(cell_case(row = "sex == 1")$value, 0)
})

test_that("condition and cleaning helpers work without a session", {
  rows <- row_condition_f(data.frame(row_index = 9:11, row_lgc = c("sex == 1", "", NA)))
  expect_identical(rows$row_condition, c("sex == 1", "TRUE", "TRUE"))
  cols <- col_condition_f(data.frame(col_index = 3L,
    col_lgc = "hh.sav\nfilter(total == 1)\nweight by w\nsex == 2"))
  expect_identical(cols$col_condition, "sex == 2")
  expect_identical(extract_var("sex == 2"), "sex")
  expect_true(is.na(extract_var("invalid(")))
  expect_identical(normalize_condition_text(" x \u2265 1 "), "x >= 1")
  expect_equal(filter_second(data.frame(x = 1:3, y = c(NA, 1, 2)))$x, 2:3)
  tab <- data.frame(row_index = 3:8, check.names = FALSE,
    `1` = "label", `2` = NA_character_, `3` = "value")
  expect_equal(get_blank_cols(tab), 2L)
})


test_that("individual checks return pass, fail, and not-applicable results", {
  session <- small_plan_session()
  session$cell_results <- check_example()
  expect_identical(row_group_total_check(session)$status, "pass")
  session$cell_results$value[3] <- 1
  failed <- row_group_total_check(session)
  expect_identical(failed$status, "fail")
  expect_equal(failed$issue_count, 1)
  expect_equal(nrow(failed$issues), 1)
  session$cell_results$stat_type <- "p"
  expect_output(result <- row_group_total_check(session), "isn't")
  expect_identical(result$status, "not_applicable")
  expect_equal(result$issue_count, 0)
  expect_equal(nrow(result$details), 0)
})

test_that("all individual checks match the comprehensive check", {
  session <- small_plan_session()
  session$cell_results <- check_example()
  expect_output(all_checks <- check_mics_table(session), "isn't")
  specs <- micsPlusTableR:::mics_check_specs()
  for (i in seq_along(specs)) {
    helper <- getExportedValue("micsPlusTableR", specs[[i]][[1]])
    invisible(capture.output(single <- helper(session, diff = 1e-6)))
    expect_identical(single$status, all_checks$summary$status[i])
    expect_equal(single$issue_count, all_checks$summary$issue_count[i])
    expect_equal(single$details, all_checks$details[[i]])
  }
})

test_that("each remaining consistency check detects a real mismatch", {
  session <- small_plan_session()
  rows <- check_example()
  rows$stat_type <- "p(100)"
  rows$value <- c(100, 40, 60)
  expect_identical(row_group_perc_total_check(session, rows)$status, "pass")
  rows$value[3] <- 50
  expect_identical(row_group_perc_total_check(session, rows)$status, "fail")

  columns <- check_example()
  columns$row_index <- 9L
  columns$col_index <- 3:5
  columns$col_logic <- columns$row_logic
  columns$var_name_col <- columns$var_name_row
  expect_identical(col_group_total_check(session, columns)$status, "pass")
  columns$value[3] <- 1
  expect_identical(col_group_total_check(session, columns)$status, "fail")
  columns$stat_type <- c("p", "p", "100")
  columns$value <- c(40, 60, 100)
  columns$cond <- 1L
  expect_identical(col_group_perc_total_check(session, columns)$status, "pass")
  columns$value[2] <- 50
  expect_identical(col_group_perc_total_check(session, columns)$status, "fail")
  columns$stat_type <- "n"
  expect_output(none <- col_group_perc_total_check(session, columns), "isn't")
  expect_identical(none$status, "not_applicable")
  expect_equal(nrow(none$issues), 0)

  session$out_glob$indent_rows <- data.frame(row = 9:11, indent = c(1, 2, 2))
  rows <- check_example()
  rows$indent <- c(1, 2, 2)
  expect_identical(row_indent_group_total_check(session, rows)$status, "pass")
  rows$value[3] <- 1
  expect_identical(row_indent_group_total_check(session, rows)$status, "fail")
  rows$stat_type <- "p(100)"
  rows$value <- c(100, 40, 60)
  expect_identical(row_indent_group_perc_total_check(session, rows)$status, "pass")
  rows$value[3] <- 50
  expect_identical(row_indent_group_perc_total_check(session, rows)$status, "fail")
})

test_that("horizontal separated and adjacent filters retain their block semantics", {
  for (second_col in c(3L, 4L)) {
    s <- small_plan_session()
    s$out_glob$tab <- tibble::tibble(row_index = 9L, col_index = 3:4, stat_type = "n")
    s$out_glob$tab_c <- tibble::tibble(col_index = 3:4, col_lgc = "total == 1")
    s$out_glob$col_header <- tibble::tibble(col_index = 3:4, col_header = c("A", "B"))
    s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row,
      dplyr::mutate(s$out_glob$filter_row, col_index = second_col,
                    filter_condition = "filter(sex == 2)"))
    result <- tabulate_h(s)
    expect_equal(result$value, if (second_col == 3L) c(5, 5) else c(6, 5))
    s$out_glob$filter_row$filter_condition[2] <- "filter(bad_variable == 2)"
    expect_error(tabulate_h(s), "horizontal block 2.*bad_variable")
  }
})

test_that("transformed left operands produce one variable name for map calls", {
  expect_identical(extract_var("as.numeric(age) >= 18"), "age")
  expect_identical(extract_var("between(as.numeric(age), 18, 65)"), "age")
})

test_that("extra table dimensions and value types are explained before calculation", {
  s <- small_plan_session()
  expect_error(tabulate_extra_table(s, data.frame(label = c("A", "B"), value = 1)),
    "requires 1 rows and 2 columns")
  expect_error(tabulate_extra_table(s, data.frame(label = "A", value = "50%")),
    "value columns must be numeric")
  result <- tabulate_extra_table(s, data.frame(label = "A", value = 50))
  expect_equal(result$value, 50)
  expect_identical(s$cell_results, result)
})
