tabulate_extra_table <- function(table = table_new) {

  mics_require_columns(table, character(), "table")
  expected_rows <- nrow(out_glob$tab_r)
  expected_cols <- nrow(out_glob$tab_c) + 1L
  if (nrow(table) != expected_rows || ncol(table) != expected_cols) {
    stop("Extra table has ", nrow(table), " rows and ", ncol(table),
         " columns; worksheet '", sheet, "' requires ", expected_rows,
         " rows and ", expected_cols,
         " columns (one label column followed by values in plan order).", call. = FALSE)
  }
  if (!all(vapply(table[-1L], is.numeric, logical(1)))) {
    stop("Extra-table value columns must be numeric. Keep row labels in the first column and remove display formatting from values.", call. = FALSE)
  }
  # Extra-table values are supplied by the preparation script. Run the common
  # engine only to build the cell/layout metadata: Excel row conditions are
  # retained for review but must not be evaluated against the base hh/hl data.
  cell_results <- tabulate_mics(skip_row_conditions = TRUE)

  # Then do the extra table logic
  tab <- mics_normalize_statistics(out_glob$tab)
  tab_c <- out_glob$tab_c
  tab_r <- out_glob$tab_r
  filter_row <- out_glob$filter_row

# Sheet name is converted to table_new for processing
  table_new <- table


names(table_new)[2:ncol(table_new)] <- tab_c$col_index
table_new[,1] <- tab_r$row_index
names(table_new)[1] <- "row_index"

table_new <- table_new |> 
  pivot_longer(cols = -row_index, names_to = "col_index", values_to = "value") |> 
  mutate(col_index = as.numeric(col_index))

cell_results2 <- cell_results |> select(-c(value, value_f, n_unw, col_start_1, col_end_1)) |> 
  left_join(table_new, by = c("row_index", "col_index")) 

# Format the values and create the value_f column
# How many ns in columns
count_col_ns <- tab |> distinct(col_index, stat_type) |> filter(stat_type == "n") |> nrow()

# If there are multiple blocks, and as many ns, then we use the respective n_unw for each block
  if (filter_row |> nrow() > 1 && count_col_ns == filter_row |> nrow()) {
  
  df_n_unw <- cell_results2 %>%
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = col_start,
                  col_end_1   = col_end)

# If there are multiple blocks, but only one n, then we use the only n_unw for all table
  } else if (filter_row |> nrow() > 1 && count_col_ns == 1) {

    df_n_unw <- cell_results2 %>%
    dplyr::mutate(min_col_start = min(col_start)) |> 
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = min_col_start,
                  col_end_1   = col_end)
  
# Other conditions, to be specified in a new case
  } else {

  df_n_unw <- cell_results2 %>%
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = col_start,
                  col_end_1   = col_end)

  }

  
  results1 <- cell_results2 %>%
    dplyr::left_join(
      df_n_unw,
      by = dplyr::join_by(
        row_index == row_index,
        dplyr::between(col_index, col_start_1, col_end_1)
      )
    ) |>
    restore_suppression_basis()
  



out <-
    results1 |> 
    dplyr::mutate(
      value_f_org = dplyr::case_when(
        mics_missing_value_dash(value, stat_type) ~ "-",
        stat_type %in% c("n", "n_unw", "n2", "n_unw2", "hhmembers") ~ as.character(format(round(value, 0), big.mark = ",")),
        stat_type %in% c("p", "p_unw", "p(100)", "p_unw(100)", "mean", "mean_unw") ~ as.character(round(value, 1)),
        TRUE ~ as.character(value)
      ),
      value_f_org = format_mean_display(value, stat_type, value_f_org)
    )

  # Supplied tables follow the same plan eligibility rule as calculated tables.
  # A count elsewhere in the table does not enable suppression.
  if (isTRUE(out_glob$is_supp)) {
    out <- out |>
    dplyr::mutate(
      value_f = dplyr::case_when(
        mics_missing_value_dash(value, stat_type) ~ "-",
        (is.na(n_unw) | n_unw == 0) & stat_type == "100" ~ "0",
        stat_type %in% c("n", "n_unw", "100") ~ as.character(value),
        (is.na(n_unw) | n_unw == 0) & stat_type != "100" ~ "-",
        n_unw < 25                            ~ "(*)",
        dplyr::between(n_unw, 25, 49)         ~ paste0("(", round(value, 1), ")"),
        n_unw >= 50                           ~ as.character(value),
        TRUE                                  ~ as.character(value)
      )
    ) |> 
       dplyr::mutate(
      value_f_view = dplyr::case_when(
        mics_missing_value_dash(value, stat_type) ~ "-",
        stat_type %in% c("n", "n_unw") ~ value_f_org,
        (is.na(n_unw) | n_unw == 0) & stat_type == "100" ~ "0",
        (is.na(n_unw) | n_unw == 0) & stat_type != "100" ~ "-",
        n_unw < 25                            ~ "(*)",
        dplyr::between(n_unw, 25, 49)         ~ paste0("(", value_f_org, ")"),
        n_unw >= 50                           ~ value_f_org,
        TRUE                                  ~ value_f_org
      )
    )
  } else {
    out <- out |>
      dplyr::mutate(value_f = value_f_org, value_f_view = value_f_org)
  }

  
  # Add the variable names to the output
  out <-
  out %>%
  mutate(var_name_row = purrr::map_chr(row_logic, extract_var)) 
  
  out <-
    out %>%
    mutate(var_name_col = purrr::map_chr(col_logic, extract_var)) 

return(mics_apply_display_digits(out))

}





  
