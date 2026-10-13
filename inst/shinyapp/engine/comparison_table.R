#' Build a wide table for comparison
#'
#' Uses session cell_results, excluding n_unw cells and retaining formatted
#' values.
#'
#' @return A wide tibble with row_index and one column per worksheet column.
comparison_table <- function() {
  cell_results |> 
    filter(stat_type != "n_unw") |> 
    select(row_index,
           col_index,
           value_f) |> 
    pivot_wider(names_from = col_index,
                values_from = value_f)
  
}