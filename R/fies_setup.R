# Resolve paths supplied by the app before running a survey's FIES script.
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
