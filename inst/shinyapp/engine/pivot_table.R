pivot_table <- function(table = cell_results, formatted = FALSE, type = "index") {
  
  if (formatted == TRUE) {
  out <-
    table |> 
    select(row_index,
           col_index,
           value_f) |> 
    pivot_wider(names_from = col_index,
                values_from = value_f)
    
  } else if (formatted == FALSE) {
  out <-
    table |>
    select(row_index,
           col_index,
           value) |> 
    pivot_wider(names_from = col_index,
                values_from = value)
  } else if (formatted == "view") {
  
    out <-
    table |>
    select(row_index,
           col_index,
           value_f_view) |> 
    pivot_wider(names_from = col_index,
                values_from = value_f_view)
  }

  if (type == "index") {

   out <- out

  } else if (type == "header") {

   table_names <- 
     cell_results |> 
     distinct(col_index) |> 
     left_join(out_glob$header_1, by = c("col_index")) |> 
     arrange(col_index) |> 
     pull(label)
 
    new_row <-
     table |> distinct(variable_exp_row_header, row_index) |> 
     arrange(row_index) |> 
     pull(variable_exp_row_header)

   out <-
     out |> 
     mutate(row_index = new_row) 

   names(out) <- c("row_index", table_names)




  } else if (type == "logic") {

   table_names <- 
     table |> filter(row_index == min(table$row_index)) |> 
     distinct(col_logic, col_index) |> 
     arrange(col_index) |> 
     pull(col_logic)
 
    new_row <-
     table |> distinct(row_logic, row_index) |> 
     arrange(row_index) |> 
     pull(row_logic)

   out <-
     out |> 
     mutate(row_index = new_row)

   names(out) <- c("row_index", table_names)




  }

   return(out)

}












