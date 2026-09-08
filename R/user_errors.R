mics_require_columns <- function(x, columns, argument) {
  if (!is.data.frame(x)) {
    stop("'", argument, "' must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    stop("'", argument, "' is missing required columns: ",
         paste(missing, collapse = ", "),
         ". Use the parsed plan or long-format results, rather than a pivoted table.",
         call. = FALSE)
  }
  invisible(x)
}

mics_scalar_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("'", argument, "' must be TRUE or FALSE.", call. = FALSE)
  }
}

mics_tolerance <- function(x, argument = "tolerance") {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    stop("'", argument, "' must be one finite, non-negative number.", call. = FALSE)
  }
}

mics_file <- function(path, argument) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("'", argument, "' must be one non-empty file path.", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("File for '", argument, "' does not exist: ", path,
         ". Check the path and select an existing file.", call. = FALSE)
  }
}

mics_sheet <- function(path, sheet) {
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
    mics_abort_context(e, paste0("Cannot open workbook '", path, "'"))
  })
  valid <- length(sheet) == 1L && !is.na(sheet) &&
    ((is.character(sheet) && sheet %in% sheets) ||
       (is.numeric(sheet) && is.finite(sheet) && sheet == floor(sheet) &&
          sheet >= 1 && sheet <= length(sheets)))
  if (!valid) {
    stop("'sheet' must identify exactly one worksheet in '", path,
         "'. Available sheets: ", paste(sheets, collapse = ", "), ".",
         call. = FALSE)
  }
  invisible(sheet)
}

mics_abort_context <- function(error, context) {
  # Unwrap indexed map/dplyr errors while retaining the specific underlying cause.
  cause <- error
  while (inherits(cause$parent, "condition") &&
         !inherits(cause, "mics_user_error")) cause <- cause$parent
  stop(structure(list(
    message = paste0(context, ":\n", conditionMessage(cause)),
    call = NULL, parent = error
  ), class = c("mics_user_error", "error", "condition")))
}

mics_sheet_context <- function(env) {
  sheet <- get0("sheet", envir = env, inherits = FALSE)
  if (is.null(sheet)) "Tabulation" else paste0("Worksheet '", sheet, "'")
}

mics_require_plan <- function(env, direction = NULL) {
  plan <- get0("out_glob", envir = env, inherits = FALSE)
  if (!is.list(plan)) {
    stop("Read a tabulation sheet with read_mics_tabulation() before calculating.",
         call. = FALSE)
  }
  if (length(plan$tab_direction) != 1L || is.na(plan$tab_direction) ||
      !plan$tab_direction %in% c("v", "h")) {
    stop("The plan has no valid tabulation direction. Check the worksheet's row and column logic.", call. = FALSE)
  }
  if (!is.null(direction) && plan$tab_direction != direction) {
    stop("This worksheet uses direction '", plan$tab_direction,
         "'. Use tabulate_", plan$tab_direction,
         "() or tabulate_mics_table() to select it automatically.", call. = FALSE)
  }
  for (name in c("tab", "tab_r", "tab_c", "filter_row")) {
    if (!is.data.frame(plan[[name]]) || !nrow(plan[[name]])) {
      stop("The plan has no usable '", name,
           "' entries. Check the worksheet's data-source, filter, and condition cells.", call. = FALSE)
    }
  }
  invisible(plan)
}

mics_data_source <- function(x, env) {
  name <- if (is.character(x) && length(x) == 1L && !is.na(x)) x else "<data frame>"
  value <- if (name != "<data frame>") get0(name, envir = env, inherits = TRUE) else x
  if (!is.data.frame(value)) {
    stop("Prepared data source '", name,
         "' is unavailable or is not a data frame. Run prepare_mics_data() or supply hh/hl to mics_session(); check the .sav name in the plan.", call. = FALSE)
  }
  value
}

mics_weight <- function(df, weight) {
  if (!is.character(weight) || length(weight) != 1L || is.na(weight) ||
      !nzchar(weight) || !weight %in% names(df)) {
    stop("Weight column '", paste(weight, collapse = ", "),
         "' was not found in the prepared data. Check 'weight by' in the plan and create that variable in the preparation script.", call. = FALSE)
  }
  if (!is.numeric(df[[weight]]) || any(!is.finite(df[[weight]]) & !is.na(df[[weight]])) ||
      any(df[[weight]] < 0, na.rm = TRUE)) {
    stop("Weight column '", weight,
         "' must contain numeric, non-negative, finite weights (or NA). Correct it in the preparation script.", call. = FALSE)
  }
}
