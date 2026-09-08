
row_indent_group_perc_total_check <- function(cell_results, diff = 1e-11) {
  

  indent_rows <- out_glob$indent_rows
  
  # We remove any indents that are 2 or higher
  cell_results <-
    cell_results |> 
    filter(indent %in% c(1,2))

  # find the groups
  indent_rows_grouped <-
    indent_rows |> 
    mutate(group = cumsum(indent == 1)) |> 
    group_by(group) |> 
    mutate(n = n()) |> 
    ungroup() |> 
    filter(n > 1)

  # OK to do this consistency check
  checker_n <- nrow(indent_rows_grouped)
  checker_n2 <- cell_results |> filter(stat_type %in% c("p(100)", "p_unw(100)")) |> nrow()

  checker <- checker_n > 0 & checker_n2 > 0

  if (isFALSE(checker)) {
    cat("There isn't a rowwise indent percentage total in this table.")
    totals_indent_row_perc_df <<- NULL
    totals_indent_row_perc_df_issue <<- NULL
    totals_indent_row_perc_df_issue_n <<- 0
  } else {
  
  cell_results <-
    indent_rows_grouped |> 
    left_join(cell_results, by = c("row" = "row_index", 
                                    "indent" = "indent"
    ))
  
  if (!"cond" %in% names(cell_results)) {
    
    cell_results <-
      cell_results |> 
      mutate(cond = 1)
    
  }
  
  
  # 1) overall totals (main totals)
  totals <-
    cell_results |>
    filter(
      indent == 1,
      stat_type %in% c("p", "p(100)", "p_unw", "p_unw(100)")
    ) |>
    select(cond, group, stat_type, col_index, total_value = value)
  
  # 2) group totals for every var_name_row except total/NA
  group_totals <-
    cell_results |>
    filter(
      indent > 1,
      stat_type %in% c("p", "p(100)", "p_unw", "p_unw(100)")
    ) |>
    group_by(cond, group, col_index, stat_type) |>
    summarise(group_total = sum(value), .groups = "drop")
  
  # 3) compare group totals vs overall totals
  totals_indent_row_perc_df <-
    group_totals |>
    left_join(totals, by = c("cond", "group", "col_index", "stat_type")) |>
    mutate(diff_value = total_value - group_total)
  
  totals_indent_row_perc_df <<- totals_indent_row_perc_df
  
  totals_indent_row_perc_df_issue <-
    totals_indent_row_perc_df |>
    filter(!is.na(total_value), abs(diff_value) > diff)
  
  totals_indent_row_perc_df_issue_n <<- nrow(totals_indent_row_perc_df_issue)
  
  totals_indent_row_perc_df_issue
  
  }
}


