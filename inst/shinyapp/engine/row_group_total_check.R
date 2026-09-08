
row_group_total_check <- function(cell_results, diff = 1e-11) {
  
  # We remove any indents that are 2 or higher
  cell_results <-
    cell_results |> 
    filter(indent %in% c(1,NA))
  
  # OK to do this consistency check
  checker <- cell_results |> 
     filter(row_logic == "total == 1",
            stat_type %in% c("n", "n(100)", "n_unw", "n_unw(100)"))

  checker_n <- checker |> nrow()

  if (checker_n == 0) {

    cat("There isn't a rowwise total in this table.")

    totals_row_df <<- NULL
    totals_row_df_issue <<- NULL
    totals_row_df_issue_n <<- 0

  } else { # If there is a rowwise total do the consistency check

  if (!"cond" %in% names(cell_results)) {
    
    cell_results <-
      cell_results |> 
      mutate(cond = 1)
    
  }
  
  # We check for multiple variables used in a row 
  
  is_multi_var_row <- 
    cell_results |> distinct(grp, var_name_row) |> count(grp) |> filter(n > 1) |> nrow() > 0 |
    cell_results |> filter(is.na(var_name_row)) |> nrow() > 0

  if (isFALSE(is_multi_var_row)) {
  
  # 1) overall totals (main totals)
  totals <-
    cell_results |>
    filter(
      row_logic == "total == 1",
      #col_logic %in% c("total == 1", "hhmembers"),
      stat_type %in% c("n", "n(100)", "n_unw", "n_unw(100)")
    ) |>
    select(cond, stat_type, col_index, total_value = value)
  
  # 2) group totals for every var_name_row except total/NA
  group_totals <-
    cell_results |>
    filter(
      !is.na(var_name_row),
      var_name_row != "total",
      stat_type %in% c("n", "n(100)", "n_unw", "n_unw(100)")
    ) |>
    group_by(var_name_row, cond, col_index, stat_type) |>
    summarise(group_total = sum(value), .groups = "drop")
  
  # 3) compare group totals vs overall totals
  totals_row_df <-
    group_totals |>
    left_join(totals, by = c("cond", "col_index", "stat_type")) |>
    mutate(diff_value = total_value - group_total)
  
  } else if (isTRUE(is_multi_var_row)) {

  # 1) overall totals (main totals)
  totals <-
    cell_results |>
    filter(
      row_logic == "total == 1",
      #col_logic %in% c("total == 1", "hhmembers"),
      stat_type %in% c("n", "n(100)", "n_unw", "n_unw(100)")
    ) |>
    select(cond, stat_type, col_index, total_value = value)
  
  # 2) group totals for every grp except total/NA
  group_totals <-
    cell_results |>
    filter(
      !is.na(grp),
      grp != "total",
      stat_type %in% c("n", "n(100)", "n_unw", "n_unw(100)")
    ) |>
    group_by(grp, cond, col_index, stat_type) |>
    summarise(group_total = sum(value), .groups = "drop")
  
  # 3) compare group totals vs overall totals
  totals_row_df <-
    group_totals |>
    left_join(totals, by = c("cond", "col_index", "stat_type")) |>
    mutate(diff_value = total_value - group_total)

  }
    
  totals_row_df <<- totals_row_df
  
  totals_row_df_issue <-
    totals_row_df |>
    filter(!is.na(total_value), abs(diff_value) > diff)
  
  totals_row_df_issue_n <<- nrow(totals_row_df_issue)
  
  totals_row_df_issue
  }

  }






