#' Worksheet display precision
#'
#' Every supported worksheet statistic accepts an optional named `d` argument
#' specifying the number of decimal places to display.
#' @details Use `mean(HCS8, d=0)` for a whole-number expense mean,
#' `mean(d=1)` for a mean indicator with one decimal place, `p(d=2)` for two
#' percentage decimals, or `n(d=1)` for a count with one displayed decimal.
#' Existing argument forms retain their arguments: `median(age, d=2)`,
#' `mean_unw(age, d=0)`, `p_sum(x, d=2)`, and `p(100, d=2)`.
#' A constant may use `100(d=2)`. The statistic must be supported in the table's
#' direction; adding `d` does not change the calculation or its weighting.
#'
#' `d` must be a literal non-negative whole number, supplied once. Zero means
#' no decimal point. Omit `d` to keep the existing formatting for that statistic
#' and output mode; means still default to one decimal place. Spaces around
#' arguments are allowed. `d` means decimal places, not significant figures.
#'
#' The Formatted and Suppressed Tabulator views honor explicit precision for
#' visible numbers; Unformatted retains full numeric estimates. Excel applies
#' number formats in both output workbooks, including multi-sheet runs.
#' Parentheses retain the requested precision; `(*)` and `-` remain markers.
#' Display rounding does not alter stored estimates, calculations, weights,
#' denominators, suppression thresholds, or consistency checks.
#'
#' Internally, `stat_type` contains the statistic without `d`; `display_digits`
#' holds the explicit integer or NA when omitted. Worksheet filling carries the
#' statistic and its precision together. Changing only `d` does not change mean
#' filter boundaries or the identification of `n_unw` before IDX.
#' Long-format output uses the override in `DECIMALS` and display strings while
#' retaining full precision in `OBS_VALUE`.
#' @seealso [tabulate_h()], [tabulate_v()], [write_mics_table()]
#' @name statistic-precision
NULL

# Keep display metadata separate from statistic names used by the engine.
mics_stat_spec <- function(stat_type) {
  out <- list(calculation = stat_type, digits = NA_integer_)
  if (is.na(stat_type) || !grepl("\\bd\\s*=", stat_type, perl = TRUE)) return(out)
  text <- trimws(stat_type)
  open <- regexpr("(", text, fixed = TRUE)[[1L]]
  invalid <- function() stop("Invalid statistic '", stat_type,
    "': d must be supplied once as a non-negative whole number, e.g. p(d=2).",
    call. = FALSE)
  if (open < 2L) return(out)
  name <- trimws(substr(text, 1L, open - 1L))
  expr <- tryCatch(parse(text = paste0(".stat", substring(text, open)),
                         keep.source = FALSE), error = function(e) invalid())
  if (length(expr) != 1L || !is.call(expr[[1L]]) ||
      !identical(expr[[1L]][[1L]], as.name(".stat"))) invalid()
  args <- as.list(expr[[1L]])[-1L]
  d <- which(names(args) == "d")
  if (!length(d)) return(out)
  if (length(d) != 1L) invalid()
  if (identical(args[d], alist(d = ))) invalid()
  digits <- tryCatch(args[[d]], error = function(e) invalid())
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
      digits < 0 || digits != floor(digits) || digits > .Machine$integer.max) invalid()
  args <- args[-d]
  out$digits <- as.integer(digits)
  if (length(args)) {
    call <- as.call(c(list(as.name(".stat")), args))
    remainder <- sub("^\\.stat", "", paste(deparse(call, width.cutoff = 500L), collapse = " "))
    out$calculation <- paste0(name, if (startsWith(text, "Mean ")) " " else "", remainder)
  } else out$calculation <- name
  out
}

mics_normalize_statistics <- function(table) {
  if (!"stat_type" %in% names(table)) return(table)
  stats <- as.character(table$stat_type)
  unique_stats <- unique(stats)
  specs <- lapply(unique_stats, function(st) tryCatch(mics_stat_spec(st), error = function(e) {
    i <- match(st, stats)
    location <- if (all(c("row_index", "col_index") %in% names(table)))
      paste0("Excel row ", table$row_index[i], ", column ", table$col_index[i], ": ") else ""
    stop(location, conditionMessage(e), call. = FALSE)
  }))
  index <- match(stats, unique_stats)
  table$stat_type <- vapply(specs, `[[`, character(1), "calculation")[index]
  digits <- vapply(specs, `[[`, integer(1), "digits")[index]
  if (!"display_digits" %in% names(table)) table$display_digits <- rep(NA_integer_, nrow(table))
  explicit <- !is.na(digits)
  table$display_digits[explicit] <- digits[explicit]
  table
}

# Apply explicit precision after the existing display/suppression rules.
mics_apply_display_digits <- function(table) {
  if (!"display_digits" %in% names(table)) return(table)
  for (field in intersect(c("value_f_org", "value_f_view", "value_f"), names(table))) {
    text <- table[[field]]
    use <- !is.na(table$display_digits) & !is.na(table$value) &
      !is.na(text) & !text %in% c("-", "(*)", "", "NA", "NaN")
    for (digits in unique(table$display_digits[use])) {
      selected <- which(use & table$display_digits == digits)
      value <- table$value[selected]
      # The established zero-base display for a constant 100 must stay zero.
      zero <- table$stat_type[selected] == "100" & text[selected] == "0"
      value[zero] <- 0
      number <- formatC(value, format = "f", digits = digits,
                        big.mark = if (field == "value_f") "" else ",", decimal.mark = ".")
      paren <- grepl("^\\(.*\\)$", text[selected])
      number[paren] <- paste0("(", number[paren], ")")
      table[[field]][selected] <- number
    }
  }
  table
}
