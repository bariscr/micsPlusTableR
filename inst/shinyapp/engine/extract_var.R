extract_var <- function(x) {
  e <- tryCatch(parse_expr(x), error = function(e) NULL)
  if (is.null(e)) return(NA_character_)
  
  # 1) between(x, ...)
  if (is_call(e, "between")) {
    vars <- all.vars(e[[2]])
    return(if (length(vars)) vars[[1L]] else NA_character_)
  }
  
  # 2) x %in% ...
  if (is_call(e, "%in%")) {
    vars <- all.vars(e[[2]])
    return(if (length(vars)) vars[[1L]] else NA_character_)
  }
  
  # 3) Comparison operators: ==, !=, >, <, >=, <=
  if (is_call(e) && call_name(e) %in% c("==", "!=", ">", "<", ">=", "<=")) {
    vars <- all.vars(e[[2]])
    return(if (length(vars)) vars[[1L]] else NA_character_)
  }
  
  # 4) Pure numeric or something without a variable
  return(NA_character_)
}






