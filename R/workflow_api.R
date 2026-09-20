validate_mics_session <- function(session) {
  if (!is.environment(session) || !inherits(session, "mics_tabulation_session")) {
    stop("'session' must be created by mics_session().", call. = FALSE)
  }
  invisible(session)
}

session_engine_function <- function(session, name) {
  validate_mics_session(session)
  value <- get0(name, envir = session, inherits = FALSE)
  if (!is.function(value)) {
    stop("Internal engine function is unavailable: ", name, call. = FALSE)
  }
  value
}

#' Create an isolated MICS Plus tabulation session
#'
#' Holds prepared data, a loaded plan, engine functions, and results. The app
#' creates its own session automatically. Use this function directly for
#' code-only workflows, diagnosis, or tests; see [offline-workflow].
#'
#' @param hh Optional prepared household data frame.
#' @param hl Optional prepared household-member data frame.
#' @return A mutable, isolated tabulation session environment.
#' @export
mics_session <- function(hh = NULL, hl = NULL) {
  if (!is.null(hh)) mics_require_columns(hh, character(), "hh")
  if (!is.null(hl)) mics_require_columns(hl, character(), "hl")
  session <- load_engine_environment()
  class(session) <- c("mics_tabulation_session", "environment")
  if (!is.null(hh)) assign("hh", hh, envir = session)
  if (!is.null(hl)) assign("hl", hl, envir = session)
  session
}

#' @export
print.mics_tabulation_session <- function(x, ...) {
  prepared <- all(vapply(c("hh", "hl"), function(name) {
    is.data.frame(get0(name, envir = x, inherits = FALSE))
  }, logical(1)))
  plan_loaded <- is.list(get0("out_glob", envir = x, inherits = FALSE))
  result <- get0("cell_results", envir = x, inherits = FALSE)

  cat("<micsPlusTableR session>\n")
  cat("  data prepared: ", if (prepared) "yes" else "no", "\n", sep = "")
  cat("  plan loaded:   ", if (plan_loaded) "yes" else "no", "\n", sep = "")
  cat("  result rows:   ", if (is.data.frame(result)) nrow(result) else 0L, "\n", sep = "")
  invisible(x)
}

#' Run a survey preparation script outside the Shiny application
#'
#' @param session A session created by [mics_session()].
#' @param hh_path Path to the household `.sav` file.
#' @param hl_path Path to the household-member `.sav` file.
#' @param prep_script Path to the trusted preparation `.R` script. A sibling
#'   `FIES-inputs` folder is detected automatically.
#' @param output_dir Main output folder. If the script's folder contains
#'   `FIES-inputs`, FIES artifacts are written to `FIES-outputs` below this
#'   folder.
#' @details Checks declared and detected preparation dependencies before running
#'   any preparation code. Maintained survey dependencies install with the package.
#'   [install_prep_dependencies()] is available for repair or extra custom-script
#'   requirements. See [prep-script-dependencies] for the separate dependency list.
#' @return The session, invisibly.
#' @export
prepare_mics_data <- function(session,
                              hh_path,
                              hl_path,
                              prep_script,
                              output_dir = NULL) {
  validate_mics_session(session)
  mics_file(hh_path, "hh_path")
  mics_file(hl_path, "hl_path")
  prep_inputs <- resolve_preparation_inputs(prep_script)
  prep_script <- prep_inputs$prep_script
  prep_dir <- prep_inputs$prep_dir
  fies_inputs_dir <- prep_inputs$fies_inputs_dir
  prep_packages <- check_prep_dependencies(prep_script, stop_on_missing = TRUE)

  paths <- c(hh_path = hh_path, hl_path = hl_path, prep_script = prep_script)
  missing <- names(paths)[!file.exists(paths)]
  if (length(missing)) {
    stop("Input file does not exist: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  fies_output_dir <- NULL
  if (!is.null(fies_inputs_dir)) {
    if (is.null(output_dir)) {
      output_dir <- get0("excel_root_default", envir = session, inherits = FALSE)
    }
    if (is.null(output_dir)) {
      output_dir <- getOption(
        "micsPlusTableR.output_dir",
        default_output_dir()
      )
    }
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    fies_output_dir <- file.path(output_dir, "FIES-outputs")
    dir.create(fies_output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(fies_output_dir)) {
      stop("The FIES output directory could not be created: ", fies_output_dir,
           call. = FALSE)
    }
    fies_output_dir <- normalizePath(
      fies_output_dir,
      winslash = "/",
      mustWork = TRUE
    )
  }

  # Clear survey-specific extra-table state before loading a new preparation
  # script into a reused session.
  old_extra_dict <- get0("extra_tables_dict", envir = session, inherits = FALSE)
  if (is.list(old_extra_dict)) {
    old_object_names <- unique(unname(unlist(old_extra_dict, use.names = FALSE)))
    old_object_names <- old_object_names[
      vapply(old_object_names, is.character, logical(1)) & nzchar(old_object_names)
    ]
    removable <- old_object_names[vapply(old_object_names, exists, logical(1),
      envir = session, inherits = FALSE
    )]
    if (length(removable)) rm(list = removable, envir = session)
  }
  assign("tables_extra", character(0), envir = session)
  assign("extra_tables_dict", list(), envir = session)

  prep_env <- new.env(parent = session)
  bind_preparation_packages(prep_packages$package, prep_env)
  prep_env$.GlobalEnv <- session
  prep_env$source <- function(file, local = parent.frame(), ...) {
    source_path <- resolve_preparation_source(
      file,
      prep_dir = prep_dir,
      fies_inputs_dir = fies_inputs_dir
    )
    base::source(source_path, local = local, ...)
  }
  prep_env$hh_path <- normalizePath(hh_path, winslash = "/", mustWork = TRUE)
  prep_env$hl_path <- normalizePath(hl_path, winslash = "/", mustWork = TRUE)
  prep_env$prep_dir <- prep_dir
  prep_env$prep_script <- prep_script
  prep_env$fies_inputs_dir <- fies_inputs_dir
  prep_env$path_fies <- fies_inputs_dir
  prep_env$fies_output_dir <- fies_output_dir
  prep_env$path_fies_output <- fies_output_dir

  for (name in c(
    "prep_dir", "prep_script", "fies_inputs_dir", "path_fies",
    "fies_output_dir", "path_fies_output"
  )) {
    assign(name, get(name, envir = prep_env, inherits = FALSE), envir = session)
  }

  old_dir <- setwd(if (is.null(fies_output_dir)) prep_dir else fies_output_dir)
  on.exit(setwd(old_dir), add = TRUE)

  tryCatch(base::sys.source(prep_script, envir = prep_env, keep.source = FALSE),
    error = function(e) mics_abort_context(e, paste0("Preparation script '", prep_script,
      "' failed. Check the named variable or expression in that script")))

  for (name in c("hh", "hl")) {
    if (!exists(name, envir = prep_env, inherits = FALSE)) {
      stop("Preparation script did not create object '", name, "'.", call. = FALSE)
    }
    value <- get(name, envir = prep_env, inherits = FALSE)
    if (!is.data.frame(value)) {
      stop("Preparation object '", name, "' must be a data frame.", call. = FALSE)
    }
    assign(name, value, envir = session)
  }

  for (name in c("tables_extra", "extra_tables_dict")) {
    if (exists(name, envir = prep_env, inherits = FALSE)) {
      assign(name, get(name, envir = prep_env, inherits = FALSE), envir = session)
    }
  }

  extra_dict <- get0("extra_tables_dict", envir = session, inherits = FALSE)
  if (is.list(extra_dict)) {
    for (object_name in unique(unname(unlist(extra_dict, use.names = FALSE)))) {
      if (is.character(object_name) && length(object_name) == 1L &&
          nzchar(object_name) && exists(object_name, envir = prep_env, inherits = FALSE)) {
        assign(
          object_name,
          get(object_name, envir = prep_env, inherits = FALSE),
          envir = session
        )
      }
    }
  }

  invisible(session)
}

resolve_preparation_inputs <- function(prep_script) {
  if (!is.character(prep_script) || length(prep_script) != 1L ||
      is.na(prep_script) || !file.exists(prep_script)) {
    stop("The preparation script does not exist.", call. = FALSE)
  }
  if (tolower(tools::file_ext(prep_script)) != "r") {
    stop("The preparation input must be an R file.", call. = FALSE)
  }
  prep_script <- normalizePath(prep_script, winslash = "/", mustWork = TRUE)
  prep_dir <- dirname(prep_script)

  child_dirs <- list.dirs(prep_dir, full.names = TRUE, recursive = FALSE)
  fies_dirs <- child_dirs[
    tolower(basename(child_dirs)) == tolower("FIES-inputs")
  ]
  if (length(fies_dirs) > 1L) {
    stop("The preparation folder contains multiple FIES-inputs folders.",
         call. = FALSE)
  }
  fies_inputs_dir <- if (length(fies_dirs)) {
    normalizePath(fies_dirs[[1L]], winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }

  list(
    prep_dir = prep_dir,
    prep_script = prep_script,
    fies_inputs_dir = fies_inputs_dir
  )
}

stage_preparation_upload <- function(uploaded_files,
                                     relative_paths,
                                     stage_dir = tempfile("mics-prep-upload-")) {
  required_columns <- c("name", "datapath")
  if (!is.data.frame(uploaded_files) || !nrow(uploaded_files) ||
      !all(required_columns %in% names(uploaded_files))) {
    stop("Select a preparation folder.", call. = FALSE)
  }

  relative_paths <- as.character(unlist(relative_paths, use.names = FALSE))
  if (length(relative_paths) != nrow(uploaded_files)) {
    stop("The browser did not provide the selected folder structure.",
         call. = FALSE)
  }

  relative_paths <- gsub("\\\\", "/", relative_paths)
  relative_paths <- sub("^\\./", "", relative_paths)
  invalid <- !nzchar(relative_paths) |
    grepl("^/|^[A-Za-z]:/", relative_paths) |
    grepl("(^|/)\\.\\.(/|$)", relative_paths)
  if (any(invalid)) {
    stop("The selected folder contains an unsafe relative path.", call. = FALSE)
  }

  parts <- strsplit(relative_paths, "/", fixed = TRUE)
  has_folder_root <- all(lengths(parts) >= 2L) &&
    length(unique(vapply(parts, `[[`, character(1), 1L))) == 1L
  folder_name <- if (has_folder_root) parts[[1L]][[1L]] else "Preparation folder"
  inside_paths <- if (has_folder_root) {
    vapply(parts, function(x) paste(x[-1L], collapse = "/"), character(1))
  } else {
    relative_paths
  }

  top_level_r <- which(
    dirname(inside_paths) == "." &
      tolower(tools::file_ext(inside_paths)) == "r"
  )
  if (!length(top_level_r)) {
    stop("The selected folder must contain a top-level preparation R file.",
         call. = FALSE)
  }

  prep_like <- top_level_r[
    grepl("prep", basename(inside_paths[top_level_r]), ignore.case = TRUE)
  ]
  main_index <- if (length(top_level_r) == 1L) {
    top_level_r
  } else if (length(prep_like) == 1L) {
    prep_like
  } else {
    candidates <- if (length(prep_like)) prep_like else top_level_r
    stop(
      "The selected folder has multiple possible preparation R files: ",
      paste(basename(inside_paths[candidates]), collapse = ", "),
      ". Keep one main prep-like R file at the folder's top level.",
      call. = FALSE
    )
  }

  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(stage_dir)) {
    stop("The uploaded preparation folder could not be staged.", call. = FALSE)
  }

  for (i in seq_len(nrow(uploaded_files))) {
    destination <- file.path(stage_dir, inside_paths[[i]])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    copied <- file.copy(uploaded_files$datapath[[i]], destination, overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop("Could not stage uploaded file: ", inside_paths[[i]], call. = FALSE)
    }
  }

  prep_script <- normalizePath(
    file.path(stage_dir, inside_paths[[main_index]]),
    winslash = "/",
    mustWork = TRUE
  )
  prep_inputs <- resolve_preparation_inputs(prep_script)

  c(
    prep_inputs,
    list(
      folder_name = folder_name,
      uploaded_file_count = nrow(uploaded_files)
    )
  )
}

resolve_preparation_source <- function(file, prep_dir, fies_inputs_dir = NULL) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("source() requires one non-empty file path.", call. = FALSE)
  }

  candidates <- c(
    file,
    file.path(prep_dir, file),
    if (!is.null(fies_inputs_dir)) file.path(fies_inputs_dir, file)
  )
  match <- candidates[file.exists(candidates)]
  if (!length(match)) {
    stop("Preparation helper file does not exist: ", file, call. = FALSE)
  }

  normalizePath(match[[1L]], winslash = "/", mustWork = TRUE)
}

#' Read one sheet from a MICS Plus tabulation plan
#'
#' Reads the source name in B4:B7, filters, weights, conditions, cell statistics,
#' headers, and layout metadata into the session. Blank statistic cells are
#' filled by the worksheet reader. Direction is horizontal when an `n_unw`
#' instruction immediately precedes IDX or a `100`/`100.0` statistic is present;
#' otherwise it is vertical. Optional `d=` does not change that decision.
#' The reader converts recognized numbered total column aliases to
#' `total == 1` and `totalHL` column aliases to `hhmembers`.
#' See [worksheet-conditions], [statistic-types], [statistic-precision],
#' and [tabulate_h()].
#'
#' @param session A session created by [mics_session()].
#' @param path_tab_excel Path to the Excel tabulation plan.
#' @param sheet Sheet name or index.
#' @return The parsed tabulation-plan context.
#' @export
read_mics_tabulation <- function(session, path_tab_excel, sheet) {
  validate_mics_session(session)
  mics_file(path_tab_excel, "path_tab_excel")
  mics_sheet(path_tab_excel, sheet)

  session_engine_function(session, "clear_previous")()
  assign(
    "path_tab_excel",
    normalizePath(path_tab_excel, winslash = "/", mustWork = TRUE),
    envir = session
  )
  assign("sheet", sheet, envir = session)
  tryCatch(session_engine_function(session, "read_tabulation")(path_tab_excel, sheet),
    error = function(e) mics_abort_context(e, paste0("Reading worksheet '", sheet, "' from '", path_tab_excel, "'")))

  context <- get0("out_glob", envir = session, inherits = FALSE)
  if (!is.list(context)) {
    stop("The tabulation plan did not produce a valid context.", call. = FALSE)
  }
  context
}

#' Get the current table title
#'
#' @param session A session with a tabulation sheet already read.
#' @return The table title as a character scalar.
#' @export
mics_table_name <- function(session) {
  validate_mics_session(session)
  if (!is.list(get0("out_glob", envir = session, inherits = FALSE))) {
    stop("Read a tabulation sheet before requesting its table name.", call. = FALSE)
  }
  session_engine_function(session, "get_table_name")()
}

#' Tabulate a MICS Plus table outside the application
#'
#' Exposes the app's calculation engine for code-only workflows and tests.
#' Selects the loaded plan's direction automatically or maps an available
#' supplied extra table, saves results in the session, and attaches variable
#' explanations. Use numeric long-format results for checks before pivoting.
#' See [statistic-types] for calculations and [offline-workflow] for an example.
#'
#' @param session A prepared tabulation session.
#' @param path_tab_excel Optional tabulation-plan path. Supply with `sheet` to
#'   read a new table before tabulating.
#' @param sheet Optional sheet name or index.
#' @return A long-format data frame containing one row per calculated cell.
#' @export
tabulate_mics_table <- function(session, path_tab_excel = NULL, sheet = NULL) {
  validate_mics_session(session)
  supplied <- c(!is.null(path_tab_excel), !is.null(sheet))
  if (any(supplied) && !all(supplied)) {
    stop("Supply both 'path_tab_excel' and 'sheet', or neither.", call. = FALSE)
  }
  if (all(supplied)) read_mics_tabulation(session, path_tab_excel, sheet)
  if (!is.list(get0("out_glob", envir = session, inherits = FALSE))) {
    stop("Read a tabulation sheet before tabulating.", call. = FALSE)
  }

  current_sheet <- get0("sheet", envir = session, inherits = FALSE)
  tables_extra <- get0("tables_extra", envir = session, inherits = FALSE)
  extra_dict <- get0("extra_tables_dict", envir = session, inherits = FALSE)

  if (is.character(tables_extra) && current_sheet %in% tables_extra) {
    object_name <- extra_dict[[as.character(current_sheet)]]
    if (is.null(object_name) || !nzchar(object_name)) {
      stop("No extra-table object is mapped for sheet '", current_sheet, "'.", call. = FALSE)
    }
    table <- get0(object_name, envir = session, inherits = FALSE)
    if (is.null(table)) {
      stop("Extra-table object is missing: ", object_name, call. = FALSE)
    }
    result <- session_engine_function(session, "tabulate_extra_table")(table)
  } else {
    result <- session_engine_function(session, "tabulate_mics")()
  }

  assign("cell_results", result, envir = session)
  result
}

#' Pivot long-format cell results for review
#'
#' @param session A tabulation session.
#' @param table Cell results; defaults to the session's latest result.
#' @param formatted `FALSE` uses raw numeric `value`; `TRUE` uses the engine's
#'   `value_f` display strings; `"view"` uses preview strings in `value_f_view`.
#' @param type `"index"` uses worksheet row/column positions, `"header"` uses
#'   descriptive labels, and `"logic"` uses row/column conditions. The choice
#'   changes labels, not calculated values.
#' @return A wide review table.
#' @export
pivot_mics_table <- function(session,
                             table = NULL,
                             formatted = FALSE,
                             type = c("index", "header", "logic")) {
  validate_mics_session(session)
  type <- match.arg(type)
  if (!(identical(formatted, TRUE) || identical(formatted, FALSE) || identical(formatted, "view"))) {
    stop("'formatted' must be TRUE, FALSE, or 'view'.", call. = FALSE)
  }
  if (is.null(table)) table <- get0("cell_results", envir = session, inherits = FALSE)
  if (!is.data.frame(table)) {
    stop("No cell results are available to pivot.", call. = FALSE)
  }
  mics_require_columns(table, c("row_index", "col_index", "value", "stat_type"), "table")
  session_engine_function(session, "pivot_table")(
    table = table,
    formatted = formatted,
    type = type
  )
}

#' Run all applicable consistency checks
#'
#' Runs the six group-count and percentage checks used by the application.
#' Direct R calls retain engine diagnostics; the app suppresses console output
#' while showing check results in its interface. Non-applicable checks are not
#' passing validations. Details retain internal column names such as
#' `diff_value`; the app displays readable labels such as Difference.
#'
#' @param session A tabulation session.
#' @param table Cell results; defaults to the session's latest result.
#' @param tolerance Maximum absolute difference treated as consistent.
#' @return A list containing a summary data frame and detailed check tables.
#' @export
check_mics_table <- function(session, table = NULL, tolerance = 1e-6) {
  validate_mics_session(session)
  if (is.null(table)) table <- get0("cell_results", envir = session, inherits = FALSE)
  if (!is.data.frame(table)) {
    stop("No cell results are available to check.", call. = FALSE)
  }
  mics_tolerance(tolerance)

  checks <- mics_check_specs()
  results <- lapply(checks, function(spec) {
    run_mics_check(session, spec[[1]], table, tolerance)
  })
  details <- lapply(results, `[[`, "details")
  issue_count <- vapply(results, `[[`, integer(1), "issue_count")

  applicable <- vapply(details, nrow, integer(1)) > 0L
  summary <- data.frame(
    check = names(checks),
    status = ifelse(issue_count > 0L, "fail", ifelse(applicable, "pass", "not_applicable")),
    issue_count = issue_count,
    stringsAsFactors = FALSE
  )

  structure(list(summary = summary, details = details), class = "mics_table_checks")
}

#' Write cell results to a tabulation workbook
#'
#' @param session A tabulation session with a loaded plan.
#' @param destination Existing destination workbook. Create it with
#'   [create_excel_workbooks()] or supply a previously created workbook.
#' @param sheet Sheet name or index; defaults to the current sheet.
#' @param table Cell results; defaults to the latest result.
#' @param formatted Whether to write formatted display values.
#' @param drop_n_unw Whether to omit unweighted-count columns when formatting.
#' @details Writes to an existing workbook; this function does not create one.
#' Automatic creation for unset destinations is part of the app's Multi-Sheet
#' Tabulator workflow. Rewriting a sheet replaces its previously written values.
#' Optional worksheet `d=` arguments set Excel decimal places in both
#' output formats. Numeric values retain their precision; suppression markers
#' and parentheses retain their meaning. See [statistic-precision].
#' @return The destination path, invisibly.
#' @export
write_mics_table <- function(session,
                             destination,
                             sheet = NULL,
                             table = NULL,
                             formatted = FALSE,
                             drop_n_unw = FALSE) {
  validate_mics_session(session)
  mics_file(destination, "destination")
  if (is.null(sheet)) sheet <- get0("sheet", envir = session, inherits = FALSE)
  if (is.null(table)) table <- get0("cell_results", envir = session, inherits = FALSE)
  if (is.null(sheet) || !is.data.frame(table)) {
    stop("A current sheet and cell-results table are required.", call. = FALSE)
  }

  mics_sheet(destination, sheet)
  mics_scalar_flag(formatted, "formatted")
  mics_scalar_flag(drop_n_unw, "drop_n_unw")
  mics_require_columns(table, c("row_index", "col_index", "value", "stat_type"), "table")
  session_engine_function(session, "write_to_excel")(
    dest = destination,
    table = table,
    sheet = sheet,
    drop_n_unw = drop_n_unw,
    formatted = formatted
  )
  invisible(normalizePath(destination, winslash = "/", mustWork = TRUE))
}

#' Add standard footnotes to a tabulation workbook
#'
#' @param session A tabulation session.
#' @param destination Existing destination workbook.
#' @param sheet Sheet name or index; defaults to the current sheet.
#' @param table Cell results; defaults to the latest result.
#' @return The destination path, invisibly.
#' @export
write_mics_footnotes <- function(session, destination, sheet = NULL, table = NULL) {
  validate_mics_session(session)
  mics_file(destination, "destination")
  if (is.null(sheet)) sheet <- get0("sheet", envir = session, inherits = FALSE)
  if (is.null(table)) table <- get0("cell_results", envir = session, inherits = FALSE)
  if (is.null(sheet) || !is.data.frame(table)) {
    stop("A current sheet and cell-results table are required.", call. = FALSE)
  }

  mics_sheet(destination, sheet)
  mics_require_columns(table, c("row_index", "col_index", "stat_type"), "table")
  session_engine_function(session, "write_footnotes")(
    dest = destination,
    sheet = sheet,
    df = table
  )
  invisible(normalizePath(destination, winslash = "/", mustWork = TRUE))
}

#' Compare two generated tables
#'
#' @param previous Previous or approved table.
#' @param current Current table.
#' @param ignore_col_names Compare columns by position instead of name.
#' @param show_diff_over Minimum absolute numeric difference to report.
#' @return A data frame containing differing cells.
#' @export
compare_mics_tables <- function(previous,
                                current,
                                ignore_col_names = FALSE,
                                show_diff_over = 0) {
  engine <- mics_session()
  session_engine_function(engine, "compare_tables")(
    previous,
    current,
    ignore_col_names = ignore_col_names,
    show_diff_over = show_diff_over
  )
}

#' Read a previously generated worksheet for comparison
#'
#' @param path Excel workbook path.
#' @param sheet Sheet name or index.
#' @param skip Number of header rows to skip.
#' @return A cleaned table.
#' @export
read_previous_mics_table <- function(path, sheet, skip = 3) {
  mics_file(path, "path")
  mics_sheet(path, sheet)
  if (!is.numeric(skip) || length(skip) != 1L || !is.finite(skip) || skip < 0 || skip != floor(skip)) {
    stop("'skip' must be one non-negative whole number of header rows.", call. = FALSE)
  }
  engine <- mics_session()
  assign("path_final_table", path, envir = engine)
  assign("sheet", sheet, envir = engine)
  session_engine_function(engine, "read_prev_table")(skip = skip)
}
