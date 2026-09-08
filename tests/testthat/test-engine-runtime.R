test_that("the bundled engine loads into isolated state", {
  engine <- micsPlusTableR:::load_engine_environment()

  expect_identical(parent.env(engine), asNamespace("micsPlusTableR"))
  expect_true(exists("read_tabulation", envir = engine, inherits = FALSE))
  expect_true(exists("tabulate_mics", envir = engine, inherits = FALSE))

  engine$out_glob <- list(example = TRUE)
  get("clear_previous", envir = engine)()
  expect_null(engine$out_glob)
})

test_that("clear_previous resets all table-specific outputs", {
  engine <- micsPlusTableR:::load_engine_environment()
  state_names <- c(
    "out_glob", "sheet", "cell_results", "cell_results_all", "table_name",
    "any_issues_found", "totals_col_perc_df",
    "totals_col_perc_df_issue", "totals_col_perc_df_issue_n",
    "totals_col_df", "totals_col_df_issue", "totals_col_df_issue_n",
    "totals_row_perc_df", "totals_row_perc_df_issue",
    "totals_row_perc_df_issue_n", "totals_row_df", "totals_row_df_issue",
    "totals_row_df_issue_n", "totals_indent_row_df",
    "totals_indent_row_df_issue", "totals_indent_row_df_issue_n",
    "totals_indent_row_perc_df", "totals_indent_row_perc_df_issue",
    "totals_indent_row_perc_df_issue_n"
  )

  for (name in state_names) {
    assign(name, "stale", envir = engine)
  }

  get("clear_previous", envir = engine)()

  expect_true(all(vapply(state_names, function(name) {
    is.null(get(name, envir = engine, inherits = FALSE))
  }, logical(1))))
})

test_that("extra tables without n_unw are not entirely suppressed", {
  engine <- micsPlusTableR:::load_engine_environment()
  apply_basis <- get(
    "apply_extra_table_suppression_basis",
    envir = engine,
    inherits = FALSE
  )

  supplied <- data.frame(n_unw = NA_real_, value = c(33.8, 100, 82.8))
  no_denominator <- data.frame(stat_type = c("p", "100", "mean"))
  with_denominator <- data.frame(stat_type = c("p", "n_unw", "mean"))

  expect_true(all(is.infinite(apply_basis(supplied, no_denominator)$n_unw)))
  expect_true(all(is.na(apply_basis(supplied, with_denominator)$n_unw)))
})

test_that("Excel-authored row conditions are normalized", {
  engine <- micsPlusTableR:::load_engine_environment()
  normalize <- get(
    "normalize_condition_text",
    envir = engine,
    inherits = FALSE
  )

  expect_identical(
    normalize("\u00a0variable == \u201canyone\u201d \u2227 score \u2265 50\u00a0"),
    'variable == "anyone" & score >= 50'
  )
})

test_that("extra-table row logic is retained but not evaluated", {
  engine <- micsPlusTableR:::load_engine_environment()
  logic <- c('variable == "anyone"', 'variable == "community"')
  skipped <- FALSE

  engine$out_glob <- list(
    tab = tibble::tibble(
      row_index = c(9L, 10L),
      col_index = c(3L, 3L),
      stat_type = c("p", "p")
    ),
    tab_c = tibble::tibble(col_lgc = "total == 1", col_index = 3L),
    tab_r = tibble::tibble(row_lgc = logic, row_index = c(9L, 10L)),
    filter_row = tibble::tibble(col_index = 2L)
  )

  engine$tabulate_mics <- function(skip_row_conditions = FALSE) {
    skipped <<- skip_row_conditions
    tibble::tibble(
      row_index = c(9L, 10L),
      col_index = c(3L, 3L),
      row_logic = logic,
      col_logic = "total == 1",
      stat_type = "p",
      value = 0,
      value_f = "0",
      value_f_view = "0",
      n_unw = NA_real_,
      col_start = 3L,
      col_end = 3L,
      col_start_1 = NA_integer_,
      col_end_1 = NA_integer_
    )
  }

  supplied <- tibble::tibble(label = c("Anyone", "Community"), value = c(44, 55))
  result <- get("tabulate_extra_table", envir = engine)(supplied)

  expect_true(skipped)
  expect_identical(result$row_logic, logic)
  expect_equal(result$value, c(44, 55))
})

test_that("ph is displayed as row logic without being evaluated", {
  engine <- micsPlusTableR:::load_engine_environment()
  calculate <- get("calc_cells", envir = engine, inherits = FALSE)

  result <- calculate(
    df = tibble::tibble(total = c(1, 1, 1)),
    tab_r = tibble::tibble(row_lgc = "ph", row_index = 9L),
    tab_c3 = tibble::tibble(
      col_index = 3L,
      col_condition = "total == 1",
      col_var_name = "total_1"
    ),
    tab = tibble::tibble(row_index = 9L, col_index = 3L, stat_type = "n_unw"),
    weight_var = NA_character_,
    weighted = FALSE
  )

  expect_identical(result$row_logic, "ph")
  expect_equal(result$value, 3)
})

test_that("ph is displayed as column logic without being evaluated", {
  engine <- micsPlusTableR:::load_engine_environment()
  calculate <- get("calc_cells", envir = engine, inherits = FALSE)

  result <- calculate(
    df = tibble::tibble(total = c(1, 1, 1)),
    tab_r = tibble::tibble(row_lgc = "TRUE", row_index = 9L),
    tab_c3 = tibble::tibble(
      col_index = 3L,
      col_condition = "TRUE",
      col_logic = "ph",
      col_var_name = ".mics_placeholder_col"
    ),
    tab = tibble::tibble(row_index = 9L, col_index = 3L, stat_type = "n_unw"),
    weight_var = NA_character_,
    weighted = FALSE
  )

  expect_identical(result$col_logic, "ph")
  expect_equal(result$value, 3)
})

test_that("the packaged Shiny application parses", {
  old_options <- options(micsPlusTableR.output_dir = tempfile("app-output-"))
  on.exit(options(old_options), add = TRUE)

  app_file <- system.file("shinyapp", "app.R", package = "micsPlusTableR")
  expect_true(file.exists(app_file))
  expect_no_error(parse(app_file))

  guide_file <- system.file(
    "doc", "user-guide.html",
    package = "micsPlusTableR"
  )
  expect_true(file.exists(guide_file))
  expect_gt(file.info(guide_file)$size, 0)
  guide_source <- system.file("doc", "user-guide.qmd", package = "micsPlusTableR")
  expect_true(file.exists(guide_source))
  expect_true(any(grepl(
    'src = paste0(guide_resource_prefix, "/user-guide.html")',
    readLines(app_file, warn = FALSE),
    fixed = TRUE
  )))
  app_lines <- readLines(app_file, warn = FALSE)
  expect_true(any(grepl(
    "stage_preparation_upload_app <- function",
    app_lines,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "micsPlusTableR:::stage_preparation_upload",
    app_lines,
    fixed = TRUE
  )))

  app <- shiny::as.shiny.appobj(dirname(app_file))
  expect_s3_class(app, "shiny.appobj")
  expect_no_error(shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
  }))
})
