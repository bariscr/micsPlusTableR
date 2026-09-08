tabulate_mics <- function(skip_row_conditions = FALSE) {

  mics_require_plan(environment(sys.function()))
  tab_direction <- out_glob$tab_direction
  
  if (tab_direction == "v") {
    
    result <- tabulate_v(skip_row_conditions = skip_row_conditions)
    
  } else {
    
    result <- tabulate_h(skip_row_conditions = skip_row_conditions)
    
  }

result <-
  result |> left_join(out_glob$variable_exp) |> 
  mutate(variable_exp_row_header = paste0("(", variable_exp, ") ", row_header))

 result 
  
}
