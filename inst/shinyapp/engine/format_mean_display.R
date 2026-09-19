# Format mean displays consistently with Excel's #,##0.0 number format.
# Keep other statistics and missing/suppressed-value markers unchanged.
format_mean_display <- function(value, stat_type, otherwise) {
  is_mean <- grepl("^mean($|[_ (])", trimws(stat_type), ignore.case = TRUE)
  use <- is_mean & !is.na(value)
  otherwise[use] <- formatC(value[use], format = "f", digits = 1L,
                           big.mark = ",", decimal.mark = ".")
  otherwise
}
