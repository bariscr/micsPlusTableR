# RM.weights constructs relative report names internally. Give a private copy
# of its function explicit-path writers without changing the package namespace
# or the process working directory. Its model body and report contents stay intact.
fies_model_writer <- function(model, output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must name the FIES output directory.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  output_path <- function(file) {
    if (is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file)) {
      file.path(output_dir, file)
    } else {
      file
    }
  }
  writers <- new.env(parent = environment(model))
  writers$write.table <- function(x, file = "", ...) {
    utils::write.table(x, file = output_path(file), ...)
  }
  writers$write <- function(x, file = "data", ...) {
    base::write(x, file = output_path(file), ...)
  }
  writers$cat <- function(..., file = "", sep = " ", fill = FALSE,
                          labels = NULL, append = FALSE) {
    base::cat(..., file = output_path(file), sep = sep, fill = fill,
              labels = labels, append = append)
  }
  environment(model) <- writers
  model
}

#' Fit a FIES Rasch model with explicit report paths
#'
#' Runs [RM.weights::RM.w()] with its report files directed to an explicit
#' directory. Does not change the working directory or the RM.weights namespace.
#' Model calculations, returned values, report names and contents are preserved.
#' @param ... Arguments passed to [RM.weights::RM.w()], including write.file.
#' @param output_dir Directory for generated reports; created if needed.
#'   In preparation code, use path_fies_output.
#' @return The model result returned by [RM.weights::RM.w()].
#' @seealso [fies_pc_w()], [prepare_mics_data()]
#' @export
fies_rm_w <- function(..., output_dir) {
  model <- fies_model_writer(RM.weights::RM.w, output_dir)
  model(...)
}

#' Fit a FIES partial-credit model with explicit report paths
#'
#' Runs [RM.weights::PC.w()] with its report files, including optional iteration
#' history, directed to an explicit directory. Does not change the working
#' directory or the RM.weights namespace. Model calculations and report contents
#' are preserved.
#' @param ... Arguments passed to [RM.weights::PC.w()], including write.file
#'   and write.iteration.
#' @param output_dir Directory for generated reports; created if needed.
#'   In preparation code, use path_fies_output.
#' @return The model result returned by [RM.weights::PC.w()].
#' @seealso [fies_rm_w()], [prepare_mics_data()]
#' @export
fies_pc_w <- function(..., output_dir) {
  model <- fies_model_writer(RM.weights::PC.w, output_dir)
  model(...)
}
