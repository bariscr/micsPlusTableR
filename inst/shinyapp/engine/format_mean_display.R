# Format mean displays consistently with Excel's #,##0.0 number format.
# Keep other statistics and missing/suppressed-value markers unchanged.
#' Format mean values for display
#'
#' Formats nonmissing means with one decimal place and comma thousands
#' separators; leaves other entries unchanged.
#'
#' @param value Numeric vector of calculated values.
#' @param stat_type Corresponding statistic identifiers.
#' @param otherwise Character vector of fallback display values.
#' @return The display vector with nonmissing mean entries replaced.
format_mean_display <- function(value, stat_type, otherwise) {
  is_mean <- grepl("^mean($|[_ (])", trimws(stat_type), ignore.case = TRUE)
  use <- is_mean & !is.na(value)
  otherwise[use] <- formatC(value[use], format = "f", digits = 1L,
                           big.mark = ",", decimal.mark = ".")
  otherwise
}
