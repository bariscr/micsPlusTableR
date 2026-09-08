# R/read_prev_table.R

#' Read the most recent final table sheet
#'
#' Reads the latest final tabulation table from an Excel file, skipping a given
#' number of header rows, and cleans the result.
#'
#' @param skip Integer. Number of rows to skip before the data starts in the
#'   final table sheet. Defaults to `3`.
#'
#' @return A tibble containing the cleaned table.
#'
#' @details
#' This function depends on the following **global objects** that must exist
#' in the calling environment:
#' - `path_final_table`: character path to the Excel file
#' - `sheet`: name or index of the sheet to read
#'
#' The function:
#' 1. Uses [readxl::read_excel()] to read the sheet.
#' 2. Renames the first column to `ch`.
#' 3. Filters out rows where `ch` is `NA`.
#' 4. Calls [filter_second()] to drop rows with `NA` in the second column.
#' 5. Calls [janitor::remove_empty()] to remove empty columns.
#'
#' @examples
#' \dontrun{
#' path_final_table <- "final_tables.xlsx"
#' sheet <- "Sheet1"
#' out <- read_prev_table(skip = 3)
#' }
#'
#' @seealso [filter_second()], [janitor::remove_empty()]
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr rename filter
read_prev_table <- function(skip = 3) {
  
  readxl::read_excel(path_final_table, sheet = sheet, skip = skip) |> 
    dplyr::rename(ch = 1) |> 
    dplyr::filter(!is.na(ch)) |> 
    filter_second() |> 
    janitor::remove_empty("cols") 
  
}

# Silence R CMD check about globals
utils::globalVariables(c("path_final_table", "sheet", "ch"))
