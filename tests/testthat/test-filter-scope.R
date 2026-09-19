filter_scope_session <- function(stats = rep("n", 6), local_cols = 5L,
                                 local_filters = "filter(sex == 2)") {
  s <- small_plan_session("h")
  s$hh <- data.frame(total = 1, age = c(16, 22, 30, 40),
                     sex = c(2, 2, 1, 2), yes = c(1, 1, 0, 0), w = c(10, 2, 3, 5))
  if (is.null(dim(stats))) stats <- matrix(stats, nrow = 1L)
  cols <- seq_len(ncol(stats)) + 2L
  rows <- seq_len(nrow(stats)) + 8L
  s$out_glob$tab <- tibble::tibble(
    row_index = rep(rows, each = length(cols)), col_index = rep(cols, length(rows)),
    stat_type = as.vector(t(stats)))
  s$out_glob$tab_r <- tibble::tibble(row_index = rows, row_lgc = "TRUE")
  s$out_glob$tab_c <- tibble::tibble(col_index = cols, col_lgc = "TRUE")
  s$out_glob$col_header <- tibble::tibble(col_index = cols, col_header = LETTERS[cols])
  s$out_glob$row_header <- tibble::tibble(row_index = rows, row_header = "Total")
  s$out_glob$indent_rows <- tibble::tibble(row = rows, indent = 1L)
  s$out_glob$group_info <- tibble::tibble(row = rows, grp = "total")
  s$out_glob$filter_row <- tibble::tibble(
    row_index = 4L, col_index = c(2L, local_cols), df = "hh",
    filter_condition = c("filter(age >= 18)", local_filters),
    calculation = NA_character_, weight = "w")
  s
}

test_that("B restricts every horizontal block, regardless of filter spacing", {
  for (start in c(3L, 4L, 5L)) {
    s <- filter_scope_session(local_cols = start)
    original <- s$hh
    result <- tabulate_h(s)
    expect_equal(result$value, ifelse(3:8 < start, 10, 7))
    expect_true(all(result$filt1 == "filter(age >= 18)"))
    expect_identical(s$hh, original)
    expect_equal(tabulate_h(s)$value, result$value)
  }
})

test_that("B alone survives all statistic changes", {
  s <- filter_scope_session(c("p", "n", "n_unw", "mean(age)", "median(age)", "100"),
                            integer(), character())
  expect_equal(tabulate_h(s)$value, c(100, 10, 3, 33.4, 30, 100))
})

test_that("local inheritance stops only on entering a mean independently in each row", {
  s <- filter_scope_session(rbind(c("n", "p", "mean(age)", "n", "n_unw", "n"),
                                   rep("n", 6)), local_cols = 3L)
  result <- tabulate_h(s)
  expect_equal(result$value[result$row_index == 9L], c(7, 100, 33.4, 10, 3, 10))
  expect_equal(result$value[result$row_index == 10L], rep(7, 6))
  expect_equal(result$filt2[result$row_index == 9L],
               c(rep("filter(sex == 2)", 2), rep(NA_character_, 4)))
  expect_equal(nrow(result), 12L)
})

test_that("an explicit filter restarts scope and replaces the preceding local filter", {
  s <- filter_scope_session(c("n", "n", "p", "p", "n", "n"),
                            c(3L, 5L), c("filter(sex == 2)", "filter(sex == 1)"))
  result <- tabulate_h(s)
  expect_equal(result$value, c(7, 7, 100, 100, 3, 3))
  expect_equal(result$filt2[3:4], rep("filter(sex == 1)", 2))
  s$out_glob$tab$stat_type <- "n"
  expect_equal(tabulate_h(s)$value, c(7, 7, 3, 3, 3, 3))
  s$out_glob$filter_row$filter_condition[3] <- "filter(TRUE)"
  expect_equal(tabulate_h(s)$value, c(7, 7, 10, 10, 10, 10))
})

test_that("percent denominators, row predicates and column predicates retain their roles", {
  s <- filter_scope_session(c("p", "p", "n", "n", "n_unw", "n"), local_cols = 3L)
  s$out_glob$tab_c$col_lgc <- "yes == 1"
  expect_equal(tabulate_h(s)$value, c(200/7, 200/7, 2, 2, 1, 2))
  s$out_glob$tab_r$row_lgc <- "age >= 30"
  expect_equal(tabulate_h(s)$value, rep(0, 6))
  expect_equal(tabulate_h(s, skip_row_conditions = TRUE)$value,
               c(200/7, 200/7, 2, 2, 1, 2))
})

test_that("percentages and their weighted and unweighted bases share the local population", {
  s <- filter_scope_session(c("p", "n", "n_unw", "median(age)", "n1", "100"),
                            local_cols = 3L)
  s$out_glob$tab_c$col_lgc[1] <- "yes == 1"
  result <- tabulate_h(s)
  expect_equal(result$value, c(200/7, 7, 2, 31, 7, 100))
  expect_equal(result$filt2, rep("filter(sex == 2)", 6))
  expect_equal(result$value[1], 100 * 2 / result$value[2])
})

test_that("a filter starting on a mean continues through counts until a later mean entry", {
  s <- filter_scope_session(c("mean(age)", "mean(w)", "n", "p", "mean(age)", "n_unw"),
                            local_cols = 3L)
  result <- tabulate_h(s)
  expect_equal(result$value, c(244/7, 29/7, 7, 100, 33.4, 3))
  expect_equal(result$filt2, c(rep("filter(sex == 2)", 4), NA, NA))
})

test_that("an explicit filter on a mean overrides expiration of the preceding filter", {
  s <- filter_scope_session(c("p", "n", "mean(age)", "mean(w)", "n", "n_unw"),
                            c(3L, 5L), c("filter(sex == 2)", "filter(sex == 1)"))
  result <- tabulate_h(s)
  expect_equal(result$value, c(100, 7, 30, 3, 3, 1))
  expect_equal(result$filt2, c(rep("filter(sex == 2)", 2), rep("filter(sex == 1)", 4)))
})

test_that("bare and legacy mean statistics also stop inherited local filters", {
  for (stat in c("mean", "Mean")) {
    s <- filter_scope_session(c("n", stat, "n", "n_unw"), local_cols = 3L)
    s$out_glob$tab_c$col_lgc <- "yes == 1"
    result <- tabulate_h(s)
    expect_equal(result$filt2, c("filter(sex == 2)", rep(NA_character_, 3)))
    expect_equal(result$value[c(1, 3, 4)], c(2, 2, 1))
  }
})

test_that("count variants and changes between mean variables preserve inheritance", {
  s <- filter_scope_session(c("n", "n1", "n", "n", "n", "n"), local_cols = 3L)
  expect_equal(tabulate_h(s)$value, rep(7, 6))
  s$out_glob$tab$stat_type <- c("mean(age)", "mean(w)", rep("mean(age)", 4))
  expect_equal(tabulate_h(s)$value, c(244/7, 29/7, rep(244/7, 4)))
})

test_that("calculations and weights survive expiration and mutate-only entries", {
  s <- filter_scope_session(c("n", "mean(derived)", rep("n", 4)), local_cols = 3L)
  s$out_glob$filter_row$calculation <- c("mutate(derived = age * 2)",
                                        "mutate(derived = derived + 1, w2 = w * 2)")
  s$out_glob$filter_row$weight[2] <- "w2"
  s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row,
    tibble::tibble(row_index = 4L, col_index = 4L, df = "hh",
                  filter_condition = NA_character_, calculation = "mutate(derived = derived + 2)",
                  weight = "w2"))
  result <- tabulate_h(s)
  expect_equal(result$value, c(14, 33.4 * 2 + 3, rep(20, 4)))
  expect_equal(result$weight_var, rep("w2", 6))
  expect_equal(nrow(result), 6L)
  expect_false("derived" %in% names(s$hh))
})

test_that("source switches still apply B before their local filter", {
  s <- filter_scope_session(local_cols = 5L)
  s$hl <- s$hh
  s$hl$w <- s$hl$w * 2
  s$out_glob$filter_row$df[2] <- "hl"
  expect_equal(tabulate_h(s)$value, c(10, 10, rep(14, 4)))
  s$out_glob$filter_row$filter_condition[2] <- "filter(missing_variable == 1)"
  expect_error(tabulate_h(s), "horizontal block 2.*missing_variable")
})

test_that("percentage counts and suppression retain the secondary filter", {
  s <- filter_scope_session(c("p", "n", "n_unw"), local_cols = 3L)
  s$hh <- data.frame(total = 1, age = rep(c(16, 22), c(5, 60)),
                     sex = c(rep(2, 25), rep(1, 40)), w = 1)
  s$out_glob$is_supp <- TRUE
  result <- tabulate_h(s)
  expect_equal(result$value, c(100, 20, 20))
  expect_equal(result$n_unw, rep(20, 3))
  expect_identical(result$value_f_view[1], "(*)")
})

test_that("vertical tabulation retains its primary-filter behavior", {
  s <- filter_scope_session(c("p", "n", "n_unw", "mean(age)", "median(age)", "100"))
  s$out_glob$tab_direction <- "v"
  expect_equal(tabulate_v(s)$value, c(100, 10, 3, 33.4, 30, 100))
})

test_that("Excel reading preserves global filters and resolves blank statistics before scope", {
  s <- filter_scope_session()
  cells <- matrix(NA_character_, 10, 9)
  cells[1, 1] <- "Table 1 Example"
  cells[3, ] <- c("Rows", "Conditions", LETTERS[3:8], "IDX")
  cells[4, 2] <- "hh.sav\nfilter(age >= 18)\nweight by w"
  cells[4, 3:8] <- "yes == 1"
  cells[4, 5] <- "filter(sex == 2)\n- - -\nyes == 1"
  cells[9:10, 1] <- c("Total", "Second row")
  cells[9:10, 2] <- "TRUE"
  cells[9, 3:8] <- c("p", "p", "p", "n", "p", "n_unw")
  # Row 10 deliberately leaves every statistic blank for the reader to fill.
  wb <- openxlsx2::wb_workbook()$add_worksheet("Example")$add_worksheet("IDX")
  wb$add_data("Example", cells, col_names = FALSE)
  wb$add_data("IDX", "Table 1 Example", col_names = FALSE)
  wb$add_cell_style("Example", dims = "A9:A10", horizontal = "left", indent = 1)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  wb$save(path)

  plan <- read_mics_tabulation(s, path, "Example")
  expect_identical(plan$tab_direction, "h")
  expect_equal(plan$filter_row$filter_condition,
               c("filter(age >= 18)", "filter(sex == 2)"))
  expect_equal(plan$tab$stat_type[plan$tab$row_index == 10L], cells[9, 3:8])
  result <- tabulate_mics_table(s) |>
    dplyr::filter(!is.na(stat_type)) |>
    dplyr::arrange(row_index, col_index)
  expect_equal(result$value, rep(c(20, 20, 200/7, 2, 200/7, 1), 2))
  expect_equal(nrow(result), 12L)

  # A blank statistic on the next row inherits the mean before scope is checked.
  cells[9, 7] <- "mean(age)"
  cells[4, 7] <- "TRUE"
  wb$add_data("Example", cells, col_names = FALSE)
  wb$save(path, overwrite = TRUE)
  read_mics_tabulation(s, path, "Example")
  result <- tabulate_mics_table(s) |>
    dplyr::filter(!is.na(stat_type)) |>
    dplyr::arrange(row_index, col_index)
  expect_equal(result$value, rep(c(20, 20, 200/7, 2, 33.4, 1), 2))
  expect_equal(result$filt2, rep(c(NA, NA, rep("filter(sex == 2)", 2), NA, NA), 2))
})
