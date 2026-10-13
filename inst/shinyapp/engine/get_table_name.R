#' Get the current worksheet table title
#'
#' Reads row 1, column 2 of out_glob$tab_org and assigns table_name in the
#' engine environment.
#'
#' @return The table title from the loaded worksheet.
get_table_name <- function() {

  tab_org <- out_glob$tab_org
  
  table_name <- tab_org[1,2] |> pull()
  table_name <<- table_name
  table_name
}