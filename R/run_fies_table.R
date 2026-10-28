#' Run a survey FIES script and return an extra table
#'
#' Use this inside a tabulation plan's `Preparation File` sheet. The selected
#' `FIES-inputs` folder must contain exactly one file ending in `script.R`
#' (case-insensitive, including `_Script.R` and `.script.R`). Uploaded data
#' paths are supplied automatically. The script runs in a separate environment
#' so its assignments do not replace the preparation's `hh` and `hl` objects.
#'
#' @return A data frame with `stub`, `Moderate food insecurity`, `P_sev`,
#'   `P_mod`, `count_w`, and `count_unw`, in the script's row order. Percentage
#'   columns are on a 0--100 scale. Assign this result to any object name and
#'   register that name in `extra_tables_dict`.
#' @details The script must create a local data frame named `fies_result` with
#'   character columns `group` and `category`, and numeric columns `p_mod`,
#'   `p_sev`, `count_w`, and `count_unw`. `p_mod` means moderate-or-severe
#'   prevalence and `p_sev` means severe prevalence, both on a 0--1 scale.
#'   Missing prevalence estimates may be `NA`; counts must be finite and
#'   nonnegative. Group/category pairs must be unique.
#'
#'   The survey script defines questionnaire variables, recoding, eligibility,
#'   weights, category mappings and row order. This helper does not infer or
#'   change those definitions. Rows must match the target worksheet; the extra
#'   table engine checks dimensions when that worksheet is tabulated.
#'   The helper requires the preparation context supplied by
#'   [prepare_mics_data()]. Standalone survey scripts remain independent of it.
#' @examples
#' \dontrun{
#' # In Preparation File, after preparing hh and hl:
#' food_table <- run_fies_table()
#' extra_tables_dict <- list("4.1" = "food_table")
#' tables_extra <- names(extra_tables_dict)
#' }
#' @export
run_fies_table <- function() {
  caller <- parent.frame()
  paths <- get0(".mics_fies_paths", envir = caller, inherits = TRUE)
  if (!is.list(paths) || !all(c("inputs", "hh", "output") %in% names(paths))) {
    stop("Use run_fies_table() inside Preparation File with a selected FIES-inputs folder.",
         call. = FALSE)
  }
  resolved <- fies_resolve_paths(paths$inputs, paths$hh, paths$output)
  paths[c("inputs", "hh", "output")] <- resolved[c("inputs", "hh", "output")]
  scripts <- list.files(paths$inputs, pattern = "script[.]r$",
                        ignore.case = TRUE, full.names = TRUE)
  scripts <- scripts[!dir.exists(scripts)]
  if (length(scripts) != 1L) {
    stop("Expected exactly one FIES file ending in 'script.R' in ", paths$inputs,
         "; found ", length(scripts), ".", call. = FALSE)
  }

  # The script checks this marker locally (inherits = FALSE). Do not rely on
  # parent-environment lookup or source(local = TRUE) through the prep wrapper.
  script_env <- new.env(parent = caller)
  script_env$.mics_fies_paths <- paths
  script_env$hh_path <- paths$hh
  script_env$path_fies <- paths$inputs
  script_env$path_fies_output <- paths$output
  tryCatch(base::sys.source(scripts[[1L]], envir = script_env, keep.source = FALSE),
    error = function(e) mics_abort_context(e, paste0("FIES script '", basename(scripts), "' failed")))
  result <- get0("fies_result", envir = script_env, inherits = FALSE)
  fies_result_table(result)
}

fies_result_table <- function(result) {
  required <- c("group", "category", "p_mod", "p_sev", "count_w", "count_unw")
  if (!is.data.frame(result) || !nrow(result)) {
    stop("The FIES script must create a nonempty data frame named 'fies_result'.",
         call. = FALSE)
  }
  missing <- setdiff(required, names(result))
  if (length(missing)) {
    stop("fies_result is missing columns: ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }
  for (name in c("group", "category")) {
    if (!is.character(result[[name]]) || anyNA(result[[name]]) ||
        any(!nzchar(trimws(result[[name]])))) {
      stop("fies_result$", name, " must contain nonempty character labels.", call. = FALSE)
    }
  }
  if (anyDuplicated(result[c("group", "category")])) {
    stop("fies_result has duplicate group/category pairs. Map categories in the survey script.",
         call. = FALSE)
  }
  for (name in required[-c(1L, 2L)]) {
    if (!is.numeric(result[[name]])) {
      stop("fies_result$", name, " must be numeric.", call. = FALSE)
    }
  }
  for (name in c("p_mod", "p_sev")) {
    values <- result[[name]]
    if (any(!is.na(values) & (!is.finite(values) | values < 0 | values > 1))) {
      stop("fies_result$", name, " must contain proportions between 0 and 1, or NA.",
           call. = FALSE)
    }
  }
  if (any(result$p_sev > result$p_mod, na.rm = TRUE)) {
    stop("fies_result$p_sev cannot exceed p_mod (moderate-or-severe prevalence).",
         call. = FALSE)
  }
  for (name in c("count_w", "count_unw")) {
    if (any(!is.finite(result[[name]]) | result[[name]] < 0)) {
      stop("fies_result$", name, " must contain finite, nonnegative counts.", call. = FALSE)
    }
  }
  data.frame(
    stub = result$category,
    `Moderate food insecurity` = (result$p_mod - result$p_sev) * 100,
    P_sev = result$p_sev * 100,
    P_mod = result$p_mod * 100,
    count_w = result$count_w,
    count_unw = result$count_unw,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
