comparison_table <- function() {
  cell_results |> 
    filter(stat_type != "n_unw") |> 
    select(row_index,
           col_index,
           value_f) |> 
    pivot_wider(names_from = col_index,
                values_from = value_f)
  
}