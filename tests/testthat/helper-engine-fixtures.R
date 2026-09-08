small_plan_session <- function(direction = "h") {
  session <- mics_session(hh = data.frame(total = 1, sex = c(1, 2, 2), w = c(1, 2, 3)))
  session$sheet <- "Example"
  session$out_glob <- list(
    tab_direction = direction,
    tab = tibble::tibble(row_index = 9L, col_index = 3L, stat_type = "n"),
    tab_r = tibble::tibble(row_index = 9L, row_lgc = "total == 1"),
    tab_c = tibble::tibble(col_index = 3L, col_lgc = "total == 1"),
    filter_row = tibble::tibble(row_index = 4L, col_index = 2L, df = "hh",
      filter_condition = "filter(total == 1)", calculation = NA_character_, weight = "w"),
    col_header = tibble::tibble(col_index = 3L, col_header = "Total"),
    row_header = tibble::tibble(row_index = 9L, row_header = "Total"),
    indent_rows = tibble::tibble(row = 9L, indent = 1L),
    group_info = tibble::tibble(row = 9L, grp = "total"),
    variable_exp = tibble::tibble(var_name_row = "total", variable_exp = "All"),
    is_supp = FALSE, is_total_col = TRUE
  )
  session
}

cell_case <- function(stat = "n", weighted = TRUE, row = "TRUE", col = "sex == 2",
                      df = data.frame(total = 1, sex = c(1, 2, 2), w = c(1, 2, 3))) {
  calc_cells(df,
    data.frame(row_index = 9L, row_lgc = row),
    data.frame(col_index = 3L, col_condition = col, col_var_name = "indicator"),
    data.frame(row_index = 9L, col_index = 3L, stat_type = stat), "w", weighted)
}


check_example <- function() {
  data.frame(row_index = 9:11, col_index = 3L, stat_type = "n_unw",
    value = c(3, 1, 2), row_logic = c("total == 1", "sex == 1", "sex == 2"),
    col_logic = "total == 1", var_name_row = c("total", "sex", "sex"),
    var_name_col = "total", grp = c("total", "sex", "sex"), indent = 1L)
}
