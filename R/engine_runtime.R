engine_state_names <- function() {
  c(
    "a_cells", "any_issues_found", "cell_results", "col_header",
    "condition_row_index", "condition_write", "empty_cols", "empty_rows",
    "extra_tables_dict", "filter_row", "group_info", "idx_col",
    "indent_rows", "is_n_unw", "is_supp", "is_total_col", "out_glob",
    "path_formatted_tables", "path_output_tables", "path_tab_excel",
    "row_header", "sheet", "tab", "tab_base", "tab_c", "tab_direction",
    "tab_org", "tab_r", "tables_extra"
  )
}

bind_namespace_symbols <- function(target_env) {
  namespace <- asNamespace("micsPlusTableR")
  imports <- parent.env(namespace)
  names <- unique(c(
    ls(namespace, all.names = TRUE),
    ls(imports, all.names = TRUE)
  ))
  names <- setdiff(names, c(".__NAMESPACE__.", ".__S3MethodsTable__.", ".packageName"))

  for (name in names) {
    if (!exists(name, envir = target_env, inherits = FALSE)) {
      assign(name, get(name, envir = namespace, inherits = TRUE), envir = target_env)
    }
  }
  invisible(names)
}

load_engine_environment <- function() {
  engine_dir <- system.file("shinyapp", "engine", package = "micsPlusTableR")
  if (!nzchar(engine_dir) || !dir.exists(engine_dir)) {
    stop("The internal tabulation engine could not be found.", call. = FALSE)
  }

  env <- new.env(parent = asNamespace("micsPlusTableR"))
  for (name in engine_state_names()) {
    assign(name, NULL, envir = env)
  }

  files <- sort(list.files(engine_dir, pattern = "[.]R$", full.names = TRUE))
  invisible(lapply(files, sys.source, envir = env, keep.source = FALSE))
  env
}

bind_engine_functions <- function(engine_env, target_env) {
  names <- ls(engine_env, all.names = TRUE)
  names <- names[vapply(names, function(name) {
    is.function(get(name, envir = engine_env, inherits = FALSE))
  }, logical(1))]

  for (name in names) {
    assign(name, get(name, envir = engine_env, inherits = FALSE), envir = target_env)
  }
  invisible(names)
}
