# Create a 0/1 outcome within an eligible population.
mics_dichotomize <- function(eligible = !is.na(condition), condition) {
  dplyr::case_when(
    !eligible | is.na(eligible) ~ NA_integer_,
    condition ~ 1L,
    TRUE ~ 0L
  )
}

# Test each row for a match across the supplied columns.
mics_any <- function(..., equals) {
  columns <- list(...)
  if (length(unique(lengths(columns))) != 1L) {
    stop("mics_any() needs one or more columns of the same length.", call. = FALSE)
  }
  if (length(equals) != 1L || is.na(equals)) {
    stop("'equals' must be one nonmissing value.", call. = FALSE)
  }
  Reduce(`|`, lapply(columns, function(column) column == equals))
}
