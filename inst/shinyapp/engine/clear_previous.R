# R/clear_previous.R

#' Clear previously created workflow objects
#'
#' Resets common tabulation/workflow objects in the session engine environment.
#'
#' @param env Environment to reset. If `NULL`, uses the environment in which
#'   this function was loaded (the current Shiny session engine).
#'
#' @return Invisibly returns `NULL`. Table-specific state bindings are reset to
#'   `NULL`.
#'
#' @export
clear_previous <- function(env = NULL) {
  if (is.null(env)) env <- environment(clear_previous)

  vars <- c(
    # Current table outputs
    "out_glob",
    "sheet",
    "cell_results",
    "cell_results_all",
    "table_name",
    "any_issues_found",

    # Consistency-check tables, issue rows, and issue counts
    "totals_col_perc_df",
    "totals_col_perc_df_issue",
    "totals_col_perc_df_issue_n",
    "totals_col_df",
    "totals_col_df_issue",
    "totals_col_df_issue_n",
    "totals_row_perc_df",
    "totals_row_perc_df_issue",
    "totals_row_perc_df_issue_n",
    "totals_row_df",
    "totals_row_df_issue",
    "totals_row_df_issue_n",
    "totals_indent_row_df",
    "totals_indent_row_df_issue",
    "totals_indent_row_df_issue_n",
    "totals_indent_row_perc_df",
    "totals_indent_row_perc_df_issue",
    "totals_indent_row_perc_df_issue_n"
  )

  # Keep bindings in place so legacy `<<-` assignments resolve to the session
  # engine rather than falling through to the user's global workspace.
  invisible(lapply(vars, assign, value = NULL, envir = env))

  invisible(NULL)
}
