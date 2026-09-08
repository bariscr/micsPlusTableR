get_table_name <- function() {

  tab_org <- out_glob$tab_org
  
  table_name <- tab_org[1,2] |> pull()
  table_name <<- table_name
  table_name
}