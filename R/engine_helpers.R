run_direction_tabulation <- function(session, direction, skip_row_conditions) {
  validate_mics_session(session)
  result <- session_engine_function(session, paste0("tabulate_", direction))(
    skip_row_conditions = skip_row_conditions
  )
  session$cell_results <- result
  result
}

#' Calculate a vertical table
#'
#' Filter prepared data by column and calculate row indicators using the
#' currently loaded vertical plan. The direction must match the plan.
#' Results are also saved in `session$cell_results`.
#' Use [tabulate_mics_table()] for automatic direction selection and additional
#' variable-explanation labels.
#' @details Every supported statistic accepts an optional `d=` display argument;
#' for example, `mean(x, d=0)`, `mean(d=1)`, or `p(d=2)`. Omitting it preserves
#' existing formatting. See [statistic-types] for each calculation, population,
#' weighting rule, and direction limit, and [statistic-precision] for display rules.
#' @param session A session created by [mics_session()] with prepared data and
#'   a plan loaded by [read_mics_tabulation()].
#' @param skip_row_conditions Logical; replace row predicates with TRUE during
#'   calculation while retaining their labels. Intended for extra-table testing.
#' @return A long-format cell-results data frame.
#' @seealso [tabulate_h()], [tabulate_mics_table()]
#' @export
tabulate_v <- function(session, skip_row_conditions = FALSE) {
  run_direction_tabulation(session, "v", skip_row_conditions)
}

#' Calculate a horizontal table
#'
#' Process column filter blocks and calculate column indicators using the
#' currently loaded horizontal plan. The direction must match the plan.
#' Results are also saved in `session$cell_results`.
#' Use [tabulate_mics_table()] for automatic direction selection and additional
#' variable-explanation labels.
#' @inheritParams tabulate_v
#' @details The condition-row `filter(...)` in Excel column B applies globally.
#' Each additional horizontal `filter(...)` starts at its own column and
#' combines with B. It extends right across all statistic types, including
#' means, until the next explicit `filter(...)` or `unfilter()` instruction
#' in the condition row. A new filter replaces the previous secondary filter.
#' Write `unfilter()` in place of `filter(...)` to clear the secondary filter
#' from that column onward and use only B's global filter until a new filter
#' starts. Row and column predicates still apply. Blank filter cells, changes
#' of statistic or `d=`, and changes down rows do not stop a filter.
#' Sources, calculation chains, weights, and column/row predicates retain their
#' existing roles; filters run before calculations. Vertical tabulation keeps
#' its separate primary-filter behavior; see [tabulate_v()].
#' See [statistic-types] for calculation rules. Display precision `d=` is
#' independent of filter scope; see [statistic-precision].
#' @inherit tabulate_v return
#' @seealso [tabulate_v()], [tabulate_mics_table()]
#' @export
tabulate_h <- function(session, skip_row_conditions = FALSE) {
  run_direction_tabulation(session, "h", skip_row_conditions)
}

#' Map a supplied extra table to the current plan
#' @param session A prepared session with a loaded plan.
#' @param table A data frame whose first column contains row labels and whose
#'   remaining columns contain values in plan order.
#' @return Long-format results, also saved in `session$cell_results`.
#' @details Row logic is retained as metadata, not evaluated against survey
#'   data. Values are mapped by position and the engine applies suppression.
#'   The plan's optional `d=` argument controls display precision for supplied
#'   values too; see [statistic-precision].
#' @export
tabulate_extra_table <- function(session, table) {
  validate_mics_session(session)
  mics_require_plan(session)
  mics_require_columns(table, character(), "table")
  result <- session_engine_function(session, "tabulate_extra_table")(table)
  session$cell_results <- result
  result
}

#' Calculate individual horizontal table cells
#' @param df Prepared data frame, after block filters and calculations.
#' @param tab_r Row specification with `row_index` and `row_lgc`.
#' @param tab_c3 Column specification with `col_index`, `col_condition`, and
#'   `col_var_name`; optional `col_logic` preserves display labels.
#' @param tab Cell specification with `row_index`, `col_index`, and `stat_type`.
#' @param weight_var Weight-column name. Unweighted calculations may use NA.
#'   The `p_sum` statistic always needs a weight column.
#' @param weighted FALSE for unweighted results, TRUE for weighted results,
#'   or `"both"` (legacy mode, returns the unweighted result only).
#' @return A tibble with row/column indices, logic, statistic, and numeric value.
#' @details This background helper is available for explicit cell tests and
#'   code-only work; app users normally use Tabulator. Supported statistics
#'   include counts, indicator percentages, numeric means, medians, and
#'   constant totals. See [statistic-types] for the complete horizontal forms
#'   and their populations.
#'   With `n_unw` or `n_unw1`, the column condition `hhmembers` instead sums
#'   `HLnum` for row-selected records with `total == 1`. This is a household
#'   member base from household records, not an ordinary record count.
#'   Worksheet aliases such as `total1 == 1` and `totalHL == 1` are converted
#'   by [read_mics_tabulation()], not this helper; see [worksheet-conditions].
#'
#'   Bare `mean` is an indicator percentage; `mean(variable)` averages the named
#'   variable. Median and `mean_unw` forms are unweighted, as are legacy
#'   capitalized `Mean` indicators. The `p_sum` statistic divides the variable
#'   sum by the weight sum within both conditions, then multiplies by 100.
#'
#'   Missing statistic cells return NA. An empty denominator can produce NaN.
#'   Optional `d=` arguments are returned as `display_digits` metadata and never
#'   round the calculated value; see [statistic-precision].
#' @examples
#' calc_cells(data.frame(total = c(1, 1)),
#'   data.frame(row_index = 9L, row_lgc = "TRUE"),
#'   data.frame(col_index = 3L, col_condition = "total == 1", col_var_name = "total_1"),
#'   data.frame(row_index = 9L, col_index = 3L, stat_type = "n_unw"),
#'   weight_var = NA_character_)
#'
#' # Weighted count of sex == 2: 5. Use weighted = FALSE for a count of 2.
#' calc_cells(
#'   df = data.frame(total = 1, sex = c(1, 2, 2), weight = c(1, 2, 3)),
#'   tab_r = data.frame(row_index = 9L, row_lgc = "TRUE"),
#'   tab_c3 = data.frame(col_index = 3L, col_condition = "sex == 2",
#'                      col_var_name = "sex_2"),
#'   tab = data.frame(row_index = 9L, col_index = 3L, stat_type = "n"),
#'   weight_var = "weight", weighted = TRUE
#' )
#' @export
calc_cells <- function(df, tab_r, tab_c3, tab, weight_var, weighted = FALSE) {
  session_engine_function(mics_session(), "calc_cells")(
    df, tab_r, tab_c3, tab, weight_var, weighted
  )
}

#' Parse row conditions from a plan
#' @param tab_r Data frame containing `row_index` and `row_lgc`.
#' @return A tibble containing the row index, original condition, cleaned
#'   condition, generated variable name, and `calculation`.
#' @details Separates mutate calls from predicates; blank or NA row logic
#'   means TRUE. Parses text without evaluating it against survey data.
#' @seealso [col_condition_f()], [normalize_condition_text()], [worksheet-conditions]
#' @export
row_condition_f <- function(tab_r) {
  mics_require_columns(tab_r, c("row_index", "row_lgc"), "tab_r")
  session_engine_function(mics_session(), "row_condition_f")(tab_r)
}

#' Parse column conditions from a plan
#' @param tab_c Data frame containing `col_index` and `col_lgc`.
#' @return A tibble containing the column index, original condition, cleaned
#'   condition, and generated variable name.
#' @details Uses the last meaningful line after excluding source, filter,
#'   calculation, separator, and weight lines. Parses text without evaluating
#'   it against survey data.
#'   Worksheet total aliases are converted by [read_mics_tabulation()] before
#'   this parser is called; this helper alone does not convert them.
#' @seealso [row_condition_f()], [normalize_condition_text()], [worksheet-conditions]
#' @export
col_condition_f <- function(tab_c) {
  mics_require_columns(tab_c, c("col_index", "col_lgc"), "tab_c")
  session_engine_function(mics_session(), "col_condition_f")(tab_c)
}

#' Normalize condition text copied from Excel
#' @param x Character vector of conditions.
#' @return Trimmed text with smart quotes, nonbreaking spaces, and Unicode
#'   comparison/logical operators replaced by their R equivalents; NA is retained.
#' @export
normalize_condition_text <- function(x) {
  session_engine_function(mics_session(), "normalize_condition_text")(x)
}

#' Extract the leading variable from a condition
#' @param x One condition string.
#' @return A character scalar naming the left-hand operand of a comparison,
#'   between, or membership expression; NA for unsupported or invalid syntax.
#' @export
extract_var <- function(x) {
  if (!is.character(x) || length(x) != 1L) {
    stop("'x' must be one condition string.", call. = FALSE)
  }
  session_engine_function(mics_session(), "extract_var")(x)
}

#' Find blank worksheet header columns
#' @param tab Wide data frame containing `row_index` and Excel columns named
#'   `1`, `2`, etc. Header rows 3 through 8 are inspected.
#' @return Sorted integer column indices, excluding column 1. NA, empty text,
#'   and the string `NA` count as blank.
#' @export
get_blank_cols <- function(tab) {
  mics_require_columns(tab, "row_index", "tab")
  if (ncol(tab) < 2L) stop("'tab' needs worksheet columns as well as row_index.", call. = FALSE)
  session_engine_function(mics_session(), "get_blank_cols")(tab)
}

#' Remove rows with a missing second-column value
#' @param . A data frame with at least two columns.
#' @return The input data frame with rows containing NA in column two removed.
#' @examples
#' filter_second(data.frame(label = c("A", "B"), value = c(NA, 3)))
#' @export
filter_second <- function(.) {
  mics_require_columns(., character(), ".")
  if (ncol(.) < 2L) stop("'.' must have at least two columns to filter its second column.", call. = FALSE)
  session_engine_function(mics_session(), "filter_second")(.)
}

mics_check_specs <- function() {
  list(
    row_group_count = c("row_group_total_check", "totals_row_df", "totals_row_df_issue_n"),
    row_group_percent = c("row_group_perc_total_check", "totals_row_perc_df", "totals_row_perc_df_issue_n"),
    column_group_count = c("col_group_total_check", "totals_col_df", "totals_col_df_issue_n"),
    column_group_percent = c("col_group_perc_total_check", "totals_col_perc_df", "totals_col_perc_df_issue_n"),
    row_indent_count = c("row_indent_group_total_check", "totals_indent_row_df", "totals_indent_row_df_issue_n"),
    row_indent_percent = c("row_indent_group_perc_total_check", "totals_indent_row_perc_df", "totals_indent_row_perc_df_issue_n")
  )
}

run_mics_check <- function(session, name, cell_results, diff) {
  validate_mics_session(session)
  if (is.null(cell_results)) cell_results <- session$cell_results
  if (is.null(cell_results)) stop("No cell results are available to check. Run tabulate_mics_table() first.", call. = FALSE)
  mics_tolerance(diff, "diff")
  required <- c("row_index", "col_index", "stat_type", "value")
  if (grepl("^row_group", name)) required <- c(required, "indent", "grp", "row_logic", "var_name_row")
  if (name == "col_group_total_check") {
    required <- c(required, "col_logic", "var_name_col")
    if (!is.logical(session$out_glob$is_total_col) || length(session$out_glob$is_total_col) != 1L || is.na(session$out_glob$is_total_col)) {
      stop("Column count checks require a loaded plan with is_total_col metadata. Run read_mics_tabulation() first.", call. = FALSE)
    }
  }
  if (grepl("indent", name)) {
    required <- c(required, "indent")
    mics_require_columns(session$out_glob$indent_rows, c("row", "indent"), "session$out_glob$indent_rows")
  }
  mics_require_columns(cell_results, required, "cell_results")
  if (!is.numeric(cell_results$value)) stop("'cell_results$value' must be numeric. Use raw results rather than formatted display values.", call. = FALSE)
  specs <- mics_check_specs()
  spec <- specs[[which(vapply(specs, function(x) x[[1]] == name, logical(1)))]]
  session[[spec[[2]]]] <- NULL
  session[[spec[[3]]]] <- 0L
  issues <- tryCatch(session_engine_function(session, name)(cell_results, diff = diff),
    error = function(e) mics_abort_context(e, paste0("Consistency check '", name, "'")))
  details <- session[[spec[[2]]]]
  if (!is.data.frame(details)) details <- data.frame()
  if (!is.data.frame(issues)) issues <- data.frame()
  session[[paste0(spec[[2]], "_issue")]] <- issues
  count <- as.integer(session[[spec[[3]]]])
  list(status = if (count > 0L) "fail" else if (nrow(details)) "pass" else "not_applicable",
       issue_count = count, details = details, issues = issues)
}

#' Compare row-group counts with the overall total
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
row_group_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "row_group_total_check", cell_results, diff)
}

#' Check that row-group percentages sum to 100
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
row_group_perc_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "row_group_perc_total_check", cell_results, diff)
}

#' Compare column-group counts with the overall total
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
col_group_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "col_group_total_check", cell_results, diff)
}

#' Check that column-block percentages sum to 100
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
col_group_perc_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "col_group_perc_total_check", cell_results, diff)
}

#' Compare indented child counts with their parent row
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
row_indent_group_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "row_indent_group_total_check", cell_results, diff)
}

#' Compare indented child percentages with their parent row
#' @param session A tabulation session. Indent checks also require the loaded
#'   plan's `indent_rows` metadata.
#' @param cell_results Long-format numeric results, defaulting to the latest
#'   result in the session. Use the complete results with logic/group metadata.
#' @param diff Maximum absolute difference accepted as consistent.
#' @return A list with `status` (pass, fail, or not_applicable), `issue_count`,
#'   `details` (all comparisons), and `issues` (failing comparisons).
#' @details Runs only this check and updates its corresponding session check
#'   state. A missing total or distribution produces not_applicable.
#'   Use [check_mics_table()] to run all six checks together.
#' @export
row_indent_group_perc_total_check <- function(session, cell_results = NULL, diff = 1e-11) {
  run_mics_check(session, "row_indent_group_perc_total_check", cell_results, diff)
}
