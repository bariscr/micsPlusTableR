col_group_total_check <- function(cell_results, diff = 1e-11) {
  
  is_total_col <- out_glob$is_total_col

  checker_n <- cell_results |> filter(stat_type %in% c("n", "n_unw")) |> nrow()
  checker <- checker_n > 0 & is_total_col

  if (isTRUE(checker)) {

  if (!"cond" %in% names(cell_results)) {
    
    cell_results <-
      cell_results |> 
      mutate(cond = 1)
    
  }
  
  
  # 1) overall totals (main totals)
  totals <-
    cell_results |>
    filter(
      #row_logic == "total == 1",
      col_logic %in% c("total == 1", "hhmembers"),
      stat_type %in% c("n", "n_unw")
    ) |>
    select(cond, stat_type, row_index, total_value = value)
  
  # 2) col group totals for every var_name except total/NA
  group_totals <-
    cell_results |>
    filter(
      !is.na(var_name_col),
      var_name_col != "total",
      stat_type %in% c("n", "n_unw")
    ) |> 
    group_by(var_name_col, cond, stat_type, row_index) |>
    summarise(group_total = sum(value), .groups = "drop")
  
  # 3) compare group totals vs overall totals
  totals_col_df <-
    group_totals |>
    left_join(totals, by = c("cond", "stat_type", "row_index")) |>
    mutate(diff_value = total_value - group_total)
  
  totals_col_df <<- totals_col_df
  
  # Filter the rows with issues
  totals_col_df_issue <-
    totals_col_df |>
    filter(!is.na(total_value), abs(diff_value) > diff)
  
  # Get the number of rows with issues - to be used in identifying tables with issues - and we will write to Excel and paint these sheets to red
  totals_col_df_issue_n <<- nrow(totals_col_df_issue)
  
  totals_col_df_issue
  
  } else {

   cat("There isn't a columnwise total in this table.")
   totals_col_df <<- NULL
   totals_col_df_issue <<- NULL
   totals_col_df_issue_n <<- 0

  } 
    
  }

