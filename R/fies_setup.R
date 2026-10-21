# These helpers are also distributed in FIES-runtime.R for users without the app.
fies_resolve_paths <- function(input_dir, hh_path, output_dir) {
  inputs <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  resolve <- function(path) {
    if (!grepl("^(/|[A-Za-z]:[/\\\\]|~)", path)) path <- file.path(inputs, path)
    normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  }
  if (!is.character(hh_path) || length(hh_path) != 1L || is.na(hh_path) ||
      !nzchar(hh_path)) {
    stop("Set hh_path to the household data file.", call. = FALSE)
  }
  hh <- resolve(hh_path)
  if (!file.exists(hh)) {
    stop("Household data not found. Set hh_path to its path.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("Set path_fies_output to an output folder.", call. = FALSE)
  }
  output <- resolve(output_dir)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output)) {
    stop("Could not create the FIES output folder: ", output, call. = FALSE)
  }
  list(inputs = inputs,
       hh = normalizePath(hh, winslash = "/", mustWork = TRUE),
       output = normalizePath(output, winslash = "/", mustWork = TRUE))
}

fies_input_file <- function(input_dir, suffix) {
  files <- list.files(input_dir, full.names = TRUE)
  found <- files[basename(files) == suffix | endsWith(basename(files), paste0("_", suffix))]
  if (length(found) != 1L) {
    stop("Expected one FIES input ending in '", suffix, "' in ", input_dir,
         "; found ", length(found), ". Keep one set of helpers in this folder.", call. = FALSE)
  }
  found[[1L]]
}

fies_load_dependencies <- function(target_env) {
  packages <- c("memisc", "Hmisc", "survey", "RM.weights", "reshape2", "dplyr", "ggplot2")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Install the required R packages first: install.packages(c(",
         paste(sprintf('"%s"', missing), collapse = ", "), "))", call. = FALSE)
  }
  # Bind the functions in the analysis environment; no app or package attachment
  # is required, and dplyr retains precedence for common data operations.
  for (package in c("survey", "RM.weights", "reshape2", "dplyr", "ggplot2")) {
    for (name in getNamespaceExports(package)) {
      assign(name, getExportedValue(package, name), envir = target_env)
    }
  }
  invisible(NULL)
}
