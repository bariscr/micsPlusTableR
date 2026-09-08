extract_var <- function(x) {
  e <- tryCatch(parse_expr(x), error = function(e) NULL)
  if (is.null(e)) return(NA_character_)
  
  # 1) between(x, ...)
  if (is_call(e, "between")) {
    return(as.character(e[[2]]))
  }
  
  # 2) x %in% ...
  if (is_call(e, "%in%")) {
    return(as.character(e[[2]]))
  }
  
  # 3) Comparison operators: ==, !=, >, <, >=, <=
  if (is_call(e) && call_name(e) %in% c("==", "!=", ">", "<", ">=", "<=")) {
    return(as.character(e[[2]]))
  }
  
  # 4) Pure numeric or something without a variable
  return(NA_character_)
}






