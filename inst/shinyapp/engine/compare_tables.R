# R/compare_tables.R

#' Compare two tables and report differences
#'
#' Compares two rectangular tables cell-by-cell and returns only the differing
#' cells. Designed for validating a newly generated final table against a
#' previously saved one.
#'
#' The function:
#' - Verifies both inputs have identical shape (rows/cols).
#' - Optionally ignores column names and compares purely by position
#'   (`ignore_col_names = TRUE`).
#' - Normalizes strings contained **entirely in parentheses** (e.g. `"(4)"`,
#'   `"(4.0)"`, `" ( 4,0% ) "`), coercing them to a canonical `"(4.0)"` style
#'   before comparison.
#' - Parses numeric-looking strings (handles `%`, decimal commas, thousands
#'   separators) to compute numeric deltas, and keeps only differences where the
#'   absolute difference is greater than `show_diff_over`. Non-numeric cells are
#'   compared as strings (NA == NA is treated as equal).
#'
#' @param df1 A data frame / tibble: the first table (e.g., previous final table).
#' @param df2 A data frame / tibble: the second table (e.g., current final table).
#' @param ignore_col_names Logical. If `TRUE`, compare by column position only
#'   (column names may differ). Default `FALSE`.
#' @param show_diff_over Numeric threshold; show numeric differences only when
#'   `abs(df1 - df2) > show_diff_over`. Default `0`.
#'
#' @return A tibble with one row per differing cell, containing:
#' - `row`: row index
#' - `column`: column name used during comparison (or `V1..Vn` when ignoring names)
#' - `col_df1`, `col_df2`: original column names from `df1`/`df2`
#' - `is_numeric`: whether both compared cells were numeric after parsing
#' - `value_df1`, `value_df2`: display-normalized cell values
#' - `diff`, `abs_diff`: numeric difference (for numeric cells)
#'
#' @examples
#' df1 <- tibble::tibble(a = c("(4)", "10"), b = c("x", "y"))
#' df2 <- tibble::tibble(a = c("(5.0)", "10"), b = c("x", "z"))
#' compare_tables(df1, df2, show_diff_over = 0)  # shows row 1 col a, row 2 col b
#'
#' @export
#' @importFrom dplyr mutate across row_number left_join if_else transmute arrange filter
#' @importFrom tidyr pivot_longer
compare_tables <- function(df1, df2,
                           ignore_col_names = FALSE,
                           show_diff_over = 0) {
  
  mics_require_columns(df1, character(), "df1")
  mics_require_columns(df2, character(), "df2")
  mics_scalar_flag(ignore_col_names, "ignore_col_names")
  mics_tolerance(show_diff_over, "show_diff_over")
  if (!ncol(df1) || !ncol(df2)) stop("Both comparison tables must contain at least one column.", call. = FALSE)
  # helper for paratheses
  normalize_paren_1dp <- function(s) {
    s <- as.character(s)
    ix <- grepl("^\\s*\\([^)]*\\)\\s*$", s)                 # only pure "( ... )" cells
    inner <- sub("^\\s*\\(|\\)\\s*$", "", s[ix])            # strip parens
    inner <- trimws(gsub("%", "", inner))
    # handle commas: decimal-comma vs thousands
    inner <- ifelse(grepl(",", inner) & !grepl("\\.", inner),
                    gsub(",", ".", inner),
                    gsub(",", "", inner))
    num <- suppressWarnings(as.numeric(inner))
    ok  <- !is.na(num)
    s[ix][ok] <- sprintf("(%.1f)", round(num[ok], 1)) # always one decimal (e.g., 4.0)
    s
  }
  
  # basic shape checks
  if (nrow(df1) != nrow(df2)) stop("Row counts differ: previous table has ", nrow(df1), "; current table has ", nrow(df2), ". Align the data rows and check skipped headers before comparing.", call. = FALSE)
  if (ncol(df1) != ncol(df2)) stop("Column counts differ: previous table has ", ncol(df1), "; current table has ", ncol(df2), ". Align the selected columns before comparing.", call. = FALSE)
  
  n1 <- names(df1); n2 <- names(df2)
  if (!ignore_col_names && !identical(n1, n2)) {
    stop("Column names differ — set ignore_col_names = TRUE to compare by position.")
  }
  
  # add row index and coerce all data columns to character BEFORE pivot
  df1c <- df1 %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::mutate(dplyr::across(-.row, ~ as.character(.x)))
  
  df2c <- df2 %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    dplyr::mutate(dplyr::across(-.row, ~ as.character(.x)))
  
  # if ignoring names, standardize to V1..Vn by position (keep originals for report)
  if (ignore_col_names) {
    names(df1c)[names(df1c) != ".row"] <- paste0("V", seq_len(ncol(df1c) - 1))
    names(df2c)[names(df2c) != ".row"] <- paste0("V", seq_len(ncol(df2c) - 1))
  }
  
  long1 <- df1c %>%
    tidyr::pivot_longer(-.row, names_to = "column", values_to = "value_df1") %>%
    dplyr::mutate(value_df1 = normalize_paren_1dp(value_df1))     
  
  long2 <- df2c %>%
    tidyr::pivot_longer(-.row, names_to = "column", values_to = "value_df2") %>%
    dplyr::mutate(value_df2 = normalize_paren_1dp(value_df2))   
  
  # helper -------
  pparse <- function(x) {
    x <- as.character(x)
    x <- sub("^\\s*\\((.*)\\)\\s*$", "\\1", x)              
    x <- gsub("%", "", x)
    x <- ifelse(grepl(",", x) & !grepl("\\.", x),
                gsub(",", ".", x), gsub(",", "", x))
    suppressWarnings(as.numeric(x))
  }
  
  eps <- 1e-9
  # ------------------
  
  diffs <- long1 %>%
    dplyr::left_join(long2, by = c(".row", "column")) %>%
    # classify numeric-vs-numeric (strings that parse cleanly as numbers)
    dplyr::mutate(
      
      num1_raw = pparse(value_df1),
      num2_raw = pparse(value_df2),
      num1 = num1_raw,                     # compare on full precision
      num2 = num2_raw,
      both_numeric = !is.na(num1) & !is.na(num2),
      
      value_df1 = dplyr::if_else(!is.na(num1_raw) & grepl("^\\s*\\([^)]*\\)\\s*$", value_df1),
                                 sprintf("(%.1f)", round(num1_raw, 1)), value_df1),
      value_df2 = dplyr::if_else(!is.na(num2_raw) & grepl("^\\s*\\([^)]*\\)\\s*$", value_df2),
                                 sprintf("(%.1f)", round(num2_raw, 1)), value_df2),
      
      value_df1_disp = dplyr::if_else(!is.na(num1_raw) & grepl("^\\s*\\([^)]*\\)\\s*$", value_df1),
                                      sprintf("(%.1f)", round(num1_raw, 1)), value_df1),
      value_df2_disp = dplyr::if_else(!is.na(num2_raw) & grepl("^\\s*\\([^)]*\\)\\s*$", value_df2),
                                      sprintf("(%.1f)", round(num2_raw, 1)), value_df2),
      
      # numeric differences (only meaningful when both_numeric)
      diff     = dplyr::if_else(both_numeric, num1 - num2, NA_real_),
      abs_diff = dplyr::if_else(both_numeric, abs(diff), NA_real_),
      
      # character equality (treat NA==NA as equal)
      char_equal = dplyr::if_else(
        !both_numeric,
        (is.na(value_df1) & is.na(value_df2)) |
          (!is.na(value_df1) & !is.na(value_df2) & value_df1 == value_df2),
        NA
      ),
      
      # for reporting original names when ignoring names
      col_ix  = if (ignore_col_names) as.integer(sub("^V", "", column)) else NA_integer_,
      col_df1 = if (ignore_col_names) n1[col_ix] else column,
      col_df2 = if (ignore_col_names) n2[col_ix] else column
    ) %>%
    # keep only true differences:
    # - numeric cells where abs_diff > show_diff_over
    # - non-numeric cells where strings are not equal
    dplyr::filter(
      (both_numeric & abs_diff > pmax(show_diff_over, eps)) |
        (!both_numeric & !char_equal)
    ) %>%
    dplyr::transmute(
      row = .row,
      column = if (ignore_col_names) column else column,
      col_df1, col_df2,
      is_numeric = both_numeric,
      value_df1 = value_df1_disp,        # use display strings
      value_df2 = value_df2_disp,       
      diff, abs_diff
    ) %>%
    dplyr::arrange(if (ignore_col_names) column else column, row)
  
  diffs
}
