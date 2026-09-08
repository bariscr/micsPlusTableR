#' Get indices of blank header columns
#' 
#' Adapts to "wide" structure (row_index, `1`, `2`...)
#' Checks rows 3-8. Returns integer column indices where all values are NA/Empty.
#'
#' @param tab The input tibble (wide format).
#' @return Sorted integer vector of blank column indices (excluding col 1).
#' @export
get_blank_cols <- function(tab) {
  
  # 1. Select the relevant rows (3-8) based on the 'row_index' column
  #    and pivot to long format to handle the numeric column names ("1", "2", etc.)
  long_check <- tab |>
    dplyr::filter(row_index %in% 3:8) |>
    tidyr::pivot_longer(
      cols = -row_index,       # Pivot all columns except row_index
      names_to = "col_str",
      values_to = "val"
    )
  
  # 2. Convert column names to integers and apply blank logic
  results <- long_check |>
    dplyr::mutate(
      col = suppressWarnings(as.integer(col_str)),
      
      # Define what counts as "blank" in raw data:
      # 1. Real NA
      # 2. Empty string ""
      # 3. String "NA" (common in imported text files)
      is_blank_cell = is.na(val) | trimws(val) %in% c("", "NA")
    ) |>
    
    # 3. Filter out Column 1 (usually descriptive headers)
    dplyr::filter(col != 1) |>
    
    # 4. Check if *ALL* rows (3-8) for a specific column are blank
    dplyr::group_by(col) |>
    dplyr::summarise(
      all_blank = all(is_blank_cell), 
      .groups = "drop"
    ) |>
    
    # 5. Return only the columns that passed
    dplyr::filter(all_blank) |>
    dplyr::pull(col)
  
  sort(unique(results))
}