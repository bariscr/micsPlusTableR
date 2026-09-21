test_that("setup instructions and complete predicates survive separators and line breaks", {
  instruction <- paste(
    "hh.sav", "filter(total == 1)", "- - -",
    "mutate(", "  group = if_else(code %in% c(1, 2), 1, 0),",
    "  note = 'text with ) and ('", ")", "mutate(group2 = group + 1)",
    "weight by w", "- - -", "group == 1 &", "between(age, 10, 20)", sep = "\n")
  parsed <- micsPlusTableR:::mics_parse_condition_instruction(instruction)
  expect_identical(parsed$df, "hh")
  expect_identical(parsed$weight, "w")
  expect_identical(parsed$filter_condition, "filter(total == 1)")
  df <- data.frame(code = c(1, 3))
  calculated <- eval(rlang::parse_expr(paste0("df |> ", parsed$calculation)),
                     envir = list(df = df), enclos = asNamespace("dplyr"))
  expect_equal(calculated$group2, c(2, 1))
  expect_equal(calculated$note, rep("text with ) and (", 2))
  result <- col_condition_f(data.frame(col_index = 3L, col_lgc = instruction))
  expect_identical(result$col_condition, "group == 1 & between(age, 10, 20)")
  for (separator in c("- - -", "---", "-  -  -")) {
    result <- col_condition_f(data.frame(col_index = 3L,
      col_lgc = paste("unfilter()", separator, "WS1R == 1 &", "between(WS4,1,30)", sep = "\n")))
    expect_identical(result$col_condition, "WS1R == 1 & between(WS4,1,30)")
  }
  result <- col_condition_f(data.frame(col_index = 3L,
    col_lgc = "WS1R == 1 &\r\nbetween(WS4,1,30)"))
  expect_identical(result$col_condition, "WS1R == 1 & between(WS4,1,30)")
})

test_that("incomplete mutations are reported instead of silently dropping lines", {
  expect_error(col_condition_f(data.frame(col_index = 3L,
    col_lgc = "mutate(x = if_else(a, 1, 0)\n- - -\nx == 1")),
    "Invalid condition-row instruction")
})

condition_mutation_plan <- function() {
  cells <- matrix(NA_character_, 10, 9)
  cells[1, 1] <- "Table 6.2 Example"
  cells[3, ] <- c("Rows", "Conditions", LETTERS[3:8], "IDX")
  cells[4, 2] <- paste("hh.sav", "filter(HH17 == 1)", "- - -",
                       "mutate(hhmemwt = hhweight * HLnum)", "weight by hhmemwt", sep = "\n")
  cells[4, 3] <- paste("mutate(",
    "WS1R = if_else(WS1 %in% c(11, 21), 1, 0)", ")", "- - -",
    "WS1R == 1 &", "WS4 == 0", sep = "\n")
  cells[4, 4] <- "WS1R == 1 &\nbetween(WS4, 1, 30)"
  cells[4, 5] <- "total == 1"
  cells[4, 6] <- paste("mutate(WS1R = if_else(WS1 %in% c(32, 42), 1, 0))",
                       "- - -", "WS1R == 1 &", "WS4 == 0", sep = "\n")
  cells[4, 7] <- "WS1R == 1 &\nbetween(WS4, 1, 30)"
  cells[4, 8] <- "total == 1"
  cells[9:10, 1] <- c("Total", "Second row")
  cells[9:10, 2] <- "TRUE"
  cells[9, 3:8] <- c("p", "p", "n", "p", "p", "n_unw")
  cells
}

write_condition_mutation_plan <- function(cells, path) {
  wb <- openxlsx2::wb_workbook()$add_worksheet("6.2")$add_worksheet("IDX")
  wb$add_data("6.2", cells, col_names = FALSE)
  wb$add_data("IDX", "Table 6.2 Example", col_names = FALSE)
  wb$add_cell_style("6.2", dims = "A9:A10", horizontal = "left", indent = 1)
  wb$save(path, overwrite = TRUE)
}

test_that("worksheet mutations apply from their column and retain the filter count base", {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  write_condition_mutation_plan(condition_mutation_plan(), path)
  hh <- data.frame(total = 1, HH17 = c(1, 1, 1, 1, 0), hhweight = 1, HLnum = 1,
                   WS1 = c(11, 21, 32, 42, 11), WS4 = c(0, 15, 0, 15, 0))
  s <- mics_session(hh = hh)
  plan <- read_mics_tabulation(s, path, "6.2")
  expect_equal(nrow(plan$filter_row), 3L)
  result <- tabulate_h(s) |> dplyr::filter(!is.na(stat_type)) |> dplyr::arrange(row_index, col_index)
  expect_equal(result$value, rep(c(25, 25, 4, 25, 25, 4), 2))
  expect_equal(result$n_unw, rep(4, 12))
  expect_equal(length(unique(result$cond)), 1L)
  expect_true(all(is.na(result$filt2)))
  expect_false(any(grepl("32, 42", result$calc_chain[result$col_index < 6])))
  expect_true(all(grepl("32, 42", result$calc_chain[result$col_index >= 6])))
  expect_identical(s$hh, hh)
  expect_equal((tabulate_h(s) |> dplyr::filter(!is.na(stat_type)))$value, result$value)

  # The original zero-value case: every record uses the first group's codes.
  s$hh <- hh[1:2, ]
  result <- tabulate_h(s) |> dplyr::filter(!is.na(stat_type)) |> dplyr::arrange(row_index, col_index)
  expect_equal(result$value, rep(c(50, 50, 2, 0, 0, 2), 2))
})

test_that("mutations run once in column order and leave earlier estimates unchanged", {
  s <- small_plan_session()
  s$out_glob$tab <- tibble::tibble(row_index = 9L, col_index = 3:6, stat_type = "mean(x)")
  s$out_glob$tab_c <- tibble::tibble(col_index = 3:6, col_lgc = "TRUE")
  s$out_glob$col_header <- tibble::tibble(col_index = 3:6, col_header = LETTERS[3:6])
  s$hh$x <- 1
  s$out_glob$filter_row$calculation <- "mutate(x = x + 1)"
  s$out_glob$filter_row <- dplyr::bind_rows(s$out_glob$filter_row,
    tibble::tibble(row_index = 4L, col_index = c(4L, 6L), df = "hh",
      filter_condition = NA_character_, calculation = c("mutate(x = x + 10)", "mutate(x = x * 2)"),
      weight = "w"))
  expect_equal(tabulate_h(s)$value, c(2, 12, 12, 24))
  expect_equal(s$hh$x, rep(1, 3))
})
