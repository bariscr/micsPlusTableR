# Legacy unqualified calls in the supported preparation scripts and FIES helpers.
# Document this group separately; both preparation and app packages are required.
prep_symbol_packages <- function() {
  list(
    ggplot2 = c("ggplot", "aes", "geom_density", "geom_rect", "geom_segment",
                "scale_color_manual", "scale_color_gradient2", "scale_fill_gradient2",
                "guides", "xlab", "ylab", "labs", "annotate", "theme", "element_blank", "coord_flip"),
    Hmisc = "wtd.mean",
    labelled = c("val_labels", "var_label", "val_labels<-", "var_label<-"),
    reshape2 = c("melt", "dcast"),
    RM.weights = "RM.w",
    survey = c("svydesign", "svymean", "svytotal", "svyby", "svyciprop", "svyquantile"),
    srvyr = c("as_survey_design", "survey_mean", "survey_total", "survey_prop")
  )
}

prep_literal_packages <- function(x, file) {
  if (is.character(x)) return(x)
  if (is.call(x) && identical(x[[1L]], as.name("c")) &&
      all(vapply(as.list(x)[-1L], is.character, logical(1)))) {
    return(unlist(as.list(x)[-1L], use.names = FALSE))
  }
  stop("In preparation script '", file,
       "', prep_dependencies must be a literal character vector, for example c('dplyr', 'survey').",
       call. = FALSE)
}

prep_dependency_inventory <- function(prep_script) {
  inputs <- resolve_preparation_inputs(prep_script)
  pending <- c(inputs$prep_script, if (!is.null(inputs$fies_inputs_dir)) {
    list.files(inputs$fies_inputs_dir, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  })
  visited <- character()
  packages <- sources <- character()
  add <- function(package, file) {
    if (length(package) && anyNA(package)) stop("Preparation package names cannot be NA.", call. = FALSE)
    if (any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", package))) {
      stop("Invalid preparation package name in '", file, "'. Use R package names such as 'dplyr'.", call. = FALSE)
    }
    packages <<- c(packages, package)
    sources <<- c(sources, rep(basename(file), length(package)))
  }
  while (length(pending)) {
    file <- normalizePath(pending[[1L]], winslash = "/", mustWork = TRUE)
    pending <- pending[-1L]
    if (file %in% visited) next
    visited <- c(visited, file)
    code <- tryCatch(parse(file), error = function(e) {
      mics_abort_context(e, paste0("Cannot read preparation dependencies from '", file, "'"))
    })
    walk <- function(x) {
      if (is.call(x)) {
        head <- x[[1L]]
        op <- if (is.symbol(head)) as.character(head) else ""
        if (op %in% c("::", ":::")) add(as.character(x[[2L]]), file)
        # Recognize base::library and base::source as well as bare calls.
        if (is.call(head) && as.character(head[[1L]]) %in% c("::", ":::")) {
          op <- as.character(head[[3L]])
        }
        if (op %in% c("<-", "=") && is.symbol(x[[2L]]) &&
            identical(as.character(x[[2L]]), "prep_dependencies")) {
          add(prep_literal_packages(x[[3L]], file), file)
        }
        if (op %in% c("library", "require", "requireNamespace", "loadNamespace")) {
          args <- as.list(x)[-1L]
          package <- if ("package" %in% names(args)) args$package else if (length(args)) args[[1L]] else NULL
          character_only <- isTRUE(args$character.only)
          if (is.character(package)) add(package, file)
          else if (is.symbol(package) && !character_only && op %in% c("library", "require")) {
            add(as.character(package), file)
          }
        }
        if (op %in% c("source", "sys.source")) {
          args <- as.list(x)[-1L]
          path <- if ("file" %in% names(args)) args$file else if (length(args)) args[[1L]] else NULL
          if (is.character(path)) {
            helper <- resolve_preparation_source(path, dirname(file), inputs$fies_inputs_dir)
            pending <<- c(pending, helper)
          }
        }
        # Map legacy bare calls only, not local variables with the same names.
        if (is.symbol(head)) {
          for (package in names(prep_symbol_packages())) {
            if (op %in% prep_symbol_packages()[[package]]) add(package, file)
          }
        }
      }
      if (is.call(x) || is.expression(x) || is.pairlist(x)) {
        for (i in seq_along(x)) {
          if (!identical(x[[i]], quote(expr = ))) walk(x[[i]])
        }
      }
    }
    walk(code)
  }
  # R's standard packages are supplied with R, not installed separately.
  standard <- c("base", "compiler", "datasets", "graphics", "grDevices",
                "grid", "methods", "parallel", "splines", "stats", "stats4",
                "tcltk", "tools", "utils")
  wanted <- sort(setdiff(unique(packages), standard))
  data.frame(
    package = wanted,
    required_by = vapply(wanted, function(package) {
      paste(sort(unique(sources[packages == package])), collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# Explicit lazy namespace loads keep required preparation dependencies visible
# to R's dependency checks without attaching or masking app functions.
load_preparation_namespace <- function(package) {
  switch(package,
    ggplot2 = loadNamespace("ggplot2"),
    Hmisc = loadNamespace("Hmisc"),
    labelled = loadNamespace("labelled"),
    memisc = loadNamespace("memisc"),
    reshape2 = loadNamespace("reshape2"),
    RM.weights = loadNamespace("RM.weights"),
    survey = loadNamespace("survey"),
    srvyr = loadNamespace("srvyr"),
    loadNamespace(package)
  )
}

prep_package_problem <- function(package) {
  tryCatch({
    load_preparation_namespace(package)
    NA_character_
  }, error = conditionMessage)
}

#' Check dependencies of a preparation script and its helpers
#'
#' Inspect preparation code without executing it, then check whether each
#' required package can be loaded. App dependencies and survey-specific
#' preparation dependencies install together and are documented separately in
#' [prep-script-dependencies]. This is a diagnostic tool; preparation checks
#' dependencies automatically.
#' @param prep_script Path to a preparation R script.
#' @param stop_on_missing Stop with an installation command if a required
#'   package is missing or cannot be loaded. Defaults to FALSE for inspection.
#' @return A data frame with `package`, `required_by` (script basenames),
#'   `available`, and `problem` (NA when loadable).
#' @details Reads the script's literal `prep_dependencies` declaration,
#'   qualified calls, library/require calls with literal package names, and
#'   known legacy survey/FIES function calls. It also inspects all R files in
#'   the sibling `FIES-inputs` folder and recursively follows literal source
#'   paths. Dynamically computed package names and source paths require an
#'   explicit `prep_dependencies` declaration in the main script.
#'   This checks package availability, not numerical correctness or version
#'   compatibility of the survey code. It never executes preparation code.
#' @seealso [install_prep_dependencies()], [prepare_mics_data()]
#' @export
check_prep_dependencies <- function(prep_script, stop_on_missing = FALSE) {
  mics_scalar_flag(stop_on_missing, "stop_on_missing")
  result <- prep_dependency_inventory(prep_script)
  result$problem <- vapply(result$package, prep_package_problem, character(1))
  result$available <- is.na(result$problem)
  result <- result[c("package", "required_by", "available", "problem")]
  if (stop_on_missing && any(!result$available)) {
    failed <- result[!result$available, , drop = FALSE]
    lines <- paste0("- ", failed$package, " (", failed$required_by, "): ", failed$problem)
    stop("Preparation script dependencies are missing or cannot be loaded:\n",
         paste(lines, collapse = "\n"),
         "\nMaintained survey dependencies normally install with micsPlusTableR.",
         "\nTo repair this installation or install extra custom-script packages, run:\n  micsPlusTableR::install_prep_dependencies(",
         encodeString(normalizePath(prep_script, winslash = "/"), quote = '"'), ")",
         call. = FALSE)
  }
  result
}

#' Install missing preparation-script dependencies
#' @inheritParams check_prep_dependencies
#' @param repos Package repository URL, defaulting to the CRAN cloud mirror.
#' @param lib Installation library. Defaults to the first active library.
#'   It must be listed in `.libPaths()`.
#' @return The dependency check data frame, invisibly, after installation.
#' @details Maintained survey dependencies install with micsPlusTableR.
#'   This function is only needed for repair or extra custom-script packages.
#'
#'   [check_prep_dependencies()] identifies missing or unloadable packages.
#'   Only those packages are installed. Transitive required dependencies are
#'   installed by R. Preparation scripts are never executed by this function.
#'
#'   Installation happens only when this function is explicitly called.
#'
#'   [prepare_mics_data()] reports missing packages without installing them.
#' @seealso [check_prep_dependencies()], [prep-script-dependencies]
#' @export
install_prep_dependencies <- function(prep_script, repos = "https://cloud.r-project.org",
                                      lib = .libPaths()[[1L]]) {
  if (!is.character(lib) || length(lib) != 1L || is.na(lib) ||
      !normalizePath(lib, mustWork = FALSE) %in% normalizePath(.libPaths(), mustWork = FALSE)) {
    stop("'lib' must be one of the active directories in .libPaths().", call. = FALSE)
  }
  result <- check_prep_dependencies(prep_script)
  missing <- result$package[!result$available]
  if (length(missing)) utils::install.packages(missing, repos = repos, lib = lib)
  invisible(check_prep_dependencies(prep_script, stop_on_missing = TRUE))
}

bind_preparation_packages <- function(packages, target_env) {
  # Retain the package's existing dplyr/tidyr semantics when exports overlap.
  for (package in packages) {
    for (symbol in getNamespaceExports(package)) {
      if (!exists(symbol, envir = target_env, inherits = TRUE)) {
        assign(symbol, getExportedValue(package, symbol), envir = target_env)
      }
    }
  }
  invisible(target_env)
}
