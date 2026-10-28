# Read trusted R code from the plan without executing it.
read_preparation_code <- function(tab_plan) {
  mics_file(tab_plan, "tab_plan")
  if (!"Preparation File" %in% readxl::excel_sheets(tab_plan)) {
    stop("The tabulation plan must contain a sheet named 'Preparation File'. ",
         "Put the preparation R code in cell A1 or down column A.", call. = FALSE)
  }
  cells <- readxl::read_excel(tab_plan, sheet = "Preparation File",
    range = readxl::cell_limits(c(1L, 1L), c(NA, NA)),
    col_names = FALSE, col_types = "text", trim_ws = FALSE,
    .name_repair = "minimal")
  if (!ncol(cells) || !nrow(cells) || all(is.na(cells[[1L]]) | !nzchar(trimws(cells[[1L]])))) {
    stop("The 'Preparation File' sheet is empty. Put the preparation R code in column A.",
         call. = FALSE)
  }
  if (ncol(cells) > 1L && any(vapply(cells[-1L], function(x) {
    any(!is.na(x) & nzchar(trimws(x)))
  }, logical(1)))) {
    stop("Keep preparation R code in column A of 'Preparation File'; other columns must be empty.",
         call. = FALSE)
  }
  lines <- cells[[1L]]
  lines[is.na(lines)] <- ""
  code <- gsub("\r\n?", "\n", paste(lines, collapse = "\n"))
  parsed <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) {
    mics_abort_context(e, "Cannot read R code in the 'Preparation File' sheet")
  })
  if (!length(parsed)) stop("The 'Preparation File' sheet contains no R instructions.", call. = FALSE)
  list(code = code, uses_fies = preparation_uses_fies(parsed))
}

# FIES paths and helper-loading calls are executable references, not comments.
preparation_uses_fies <- function(code) {
  uses <- FALSE
  walk <- function(x) {
    if (is.symbol(x) && as.character(x) %in%
        c("path_fies", "fies_inputs_dir", "path_fies_output", "fies_output_dir")) uses <<- TRUE
    if (is.call(x)) {
      head <- x[[1L]]
      if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) head <- head[[3L]]
      if (is.symbol(head) && as.character(head) %in%
          c("source", "sys.source", "run_fies_table")) uses <<- TRUE
    }
    if (is.call(x) || is.expression(x) || is.pairlist(x)) {
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
    }
  }
  walk(code)
  uses
}

resolve_fies_folder <- function(prep_folder) {
  if (is.null(prep_folder) || !is.character(prep_folder) || length(prep_folder) != 1L ||
      is.na(prep_folder) || !dir.exists(prep_folder)) {
    stop("This plan's 'Preparation File' uses FIES helpers. Select the preparation folder containing FIES-inputs.",
         call. = FALSE)
  }
  prep_folder <- normalizePath(prep_folder, winslash = "/", mustWork = TRUE)
  dirs <- c(prep_folder, list.dirs(prep_folder, recursive = FALSE, full.names = TRUE))
  dirs <- dirs[tolower(basename(dirs)) == "fies-inputs"]
  if (length(dirs) != 1L) {
    stop("Select a preparation folder with one FIES-inputs subfolder, or select FIES-inputs itself.",
         call. = FALSE)
  }
  normalizePath(dirs[[1L]], winslash = "/", mustWork = TRUE)
}

stage_plan_preparation <- function(tab_plan, prep_folder = NULL) {
  preparation <- read_preparation_code(tab_plan)
  fies_dir <- if (preparation$uses_fies) resolve_fies_folder(prep_folder) else NULL
  stage_dir <- tempfile("mics-plan-preparation-")
  dir.create(stage_dir)
  script <- file.path(stage_dir, "Preparation File.R")
  writeLines(preparation$code, script, useBytes = TRUE)
  list(prep_script = script, prep_dir = stage_dir, fies_inputs_dir = fies_dir,
       tab_plan = normalizePath(tab_plan, winslash = "/", mustWork = TRUE),
       prep_folder = prep_folder, cleanup_dir = stage_dir)
}
