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

# Recode survey values for calculations, using explicit rules on the raw codes.
mics_recode <- function(x, zero = NULL, missing = NULL) {
  if (!is.numeric(x) && !is.logical(x)) {
    stop("mics_recode() needs a numeric or logical vector.", call. = FALSE)
  }
  if (anyNA(zero) || anyNA(missing)) {
    stop("Supply codes in 'zero' and 'missing'. Actual NAs stay missing.", call. = FALSE)
  }
  if (any(zero %in% missing)) {
    stop("A code cannot appear in both 'zero' and 'missing'.", call. = FALSE)
  }

  raw <- as.numeric(x)
  result <- as.numeric(haven::zap_missing(x))
  result[raw %in% zero] <- 0
  result[raw %in% missing] <- NA_real_
  result
}

# Sum each row, with a missing result when no value is available.
mics_sum <- function(..., zero = NULL) {
  columns <- list(...)
  if (length(unique(lengths(columns))) != 1L) {
    stop("mics_sum() needs one or more columns of the same length.", call. = FALSE)
  }

  values <- do.call(cbind, unname(lapply(columns, mics_recode, zero = zero)))
  total <- rowSums(values, na.rm = TRUE)
  total[rowSums(!is.na(values)) == 0L] <- NA_real_
  total
}
