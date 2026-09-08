row_group_perc_total_check <- function(cell_results, diff = 1e-11) {
  

    # We remove any indents that are 2 or higher
  cell_results <-
    cell_results |> 
    filter(indent %in% c(1,NA))

  # OK to do this consistency check
    checker <- cell_results |> 
     filter(row_logic == "total == 1",
            stat_type %in% c("p(100)", "p_unw(100)"))

  checker_n <- checker |> nrow()

  if (checker_n == 0) {
    cat("There isn't a rowwise percentage total in this table.")
    totals_row_perc_df <<- NULL
    totals_row_perc_df_issue <<- NULL
    totals_row_perc_df_issue_n <<- 0
  } else {
  
  if (!"cond" %in% names(cell_results)) {
    
    cell_results <-
      cell_results |> 
      mutate(cond = 1)
    
  }
  
  
  # 1) overall totals (main totals)
  totals <-
    cell_results |>
    filter(
      row_logic == "total == 1",
      #col_logic %in% c("total == 1"),
      stat_type %in% c("p(100)", "p_unw(100)")
    ) |>
    select(cond, stat_type, col_index, total_value = value)
  
  # 2) group totals for every var_name_row except total/NA
  group_totals <-
    cell_results |>
    filter(
      !is.na(grp),
      grp != "total",
      stat_type %in% c("p(100)", "p_unw(100)")
    ) |>
    group_by(grp, cond, col_index, stat_type) |>
    summarise(group_total = sum(value), .groups = "drop")
  
  # 3) compare group totals vs overall totals
  totals_row_perc_df <-
    group_totals |>
    left_join(totals, by = c("cond", "stat_type", "col_index")) |>
    mutate(diff_value = total_value - group_total)
  
  totals_row_perc_df <<- totals_row_perc_df
  
  totals_row_perc_df_issue <-
    totals_row_perc_df |>
    filter(!is.na(total_value), abs(diff_value) > diff)
  
  totals_row_perc_df_issue_n <<- nrow(totals_row_perc_df_issue)
  
  totals_row_perc_df_issue
  }
}










