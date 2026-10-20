#' Create output workbooks from a tabulation plan
#'
#' Copies the source tabulation plan to one or both standard output workbook
#' names. Existing files are never overwritten; a numeric suffix is added when
#' a generated name already exists.
#' @details Creates copies of the tabulation plan; it does not calculate or
#' populate table results. In the app, Write to Excel calls it on request, and
#' Multi-Sheet Tabulator calls it automatically for unset required destinations.
#' In R workflows, call it before [write_mics_table()] unless using an existing
#' workbook. Unrequested entries in the returned list are NULL.
#' Formatted workbooks omit the `Preparation File` and `Define` sheets. Output
#' workbooks retain them for reproducibility.
#'
#' @param path_tab_excel Existing `.xls` or `.xlsx` tabulation plan.
#' @param output_dir Directory where workbooks will be created.
#' @param country_code Short country code used in filenames.
#' @param period Survey period used in filenames.
#' @param wave Wave label used in filenames.
#' @param target One of `"output"`, `"formatted"`, or `"both"`.
#' @param time_tag Timestamp used in filenames.
#'
#' @return A named list with `output` and `formatted` paths; unrequested entries
#'   are `NULL`.
#' @export
create_excel_workbooks <- function(path_tab_excel,
                                   output_dir,
                                   country_code,
                                   period,
                                   wave,
                                   target = c("both", "output", "formatted"),
                                   time_tag = format(Sys.time(), "%Y%m%d%H%M%S")) {
  target <- match.arg(target)
  if (!file.exists(path_tab_excel)) {
    stop("Tabulation plan does not exist: ", path_tab_excel, call. = FALSE)
  }
  if (!grepl("[.]xlsx?$", path_tab_excel, ignore.case = TRUE)) {
    stop("Tabulation plan must be an .xls or .xlsx file.", call. = FALSE)
  }

  fields <- list(country_code = country_code, period = period, wave = wave)
  bad <- names(fields)[!vapply(fields, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }, logical(1))]
  if (length(bad)) {
    stop("Missing filename metadata: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  clean_period <- gsub("[()]", "", trimws(period))
  stem <- paste(trimws(country_code), "MICSPlus", clean_period, trimws(wave))

  next_available <- function(path) {
    if (!file.exists(path)) return(path)
    extension <- tools::file_ext(path)
    base <- sub(paste0("[.]", extension, "$"), "", path, ignore.case = TRUE)
    i <- 2L
    candidate <- sprintf("%s_%d.%s", base, i, extension)
    while (file.exists(candidate)) {
      i <- i + 1L
      candidate <- sprintf("%s_%d.%s", base, i, extension)
    }
    candidate
  }

  copy_one <- function(label) {
    path <- file.path(output_dir, paste0(stem, "_", label, "_", time_tag, ".xlsx"))
    path <- next_available(path)
    if (!isTRUE(file.copy(path_tab_excel, path, overwrite = FALSE))) {
      stop("Failed to create workbook: ", path, call. = FALSE)
    }
    if (identical(label, "Formatted Tables")) {
      wb <- openxlsx2::wb_load(path)
      if (any(c("Preparation File", "Define") %in% openxlsx2::wb_get_sheet_names(wb))) {
        remove_code_sheets(wb)
        wb$save(path, overwrite = TRUE)
      }
    }
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }

  list(
    output = if (target %in% c("output", "both")) copy_one("Output Tables") else NULL,
    formatted = if (target %in% c("formatted", "both")) copy_one("Formatted Tables") else NULL
  )
}

# Preserve named ranges and print areas when a code sheet sits among tables.
# openxlsx2 1.29 can corrupt definedNames during removal when the workbook
# contains existing #REF! names; handle these scoped XML entries explicitly.
remove_preparation_sheet <- function(wb) {
  remove_code_sheet(wb, "Preparation File")
}

remove_code_sheets <- function(wb) {
  for (sheet in c("Preparation File", "Define")) remove_code_sheet(wb, sheet)
  invisible(wb)
}

remove_code_sheet <- function(wb, sheet) {
  index <- match(sheet, openxlsx2::wb_get_sheet_names(wb))
  if (is.na(index)) return(invisible(wb))
  original <- wb$workbook$definedNames
  definitions <- original
  if (length(definitions)) {
    scoped <- grepl('localSheetId="[0-9]+"', definitions)
    ids <- rep(NA_integer_, length(definitions))
    ids[scoped] <- as.integer(sub('.*localSheetId="([0-9]+)".*', "\\1", definitions[scoped]))
    keep <- (is.na(ids) | ids != index - 1L) &
      !grepl(paste0("'", sheet, "'!"), definitions, fixed = TRUE) &
      !grepl(paste0(">", sheet, "!"), definitions, fixed = TRUE)
    definitions <- definitions[keep]
    ids <- ids[keep]
    for (i in which(!is.na(ids) & ids > index - 1L)) {
      definitions[i] <- sub('localSheetId="[0-9]+"',
        paste0('localSheetId="', ids[i] - 1L, '"'), definitions[i])
    }
  }
  wb$workbook$definedNames <- character()
  tryCatch(wb$remove_worksheet(sheet), error = function(e) {
    wb$workbook$definedNames <- original
    stop(e)
  })
  wb$workbook$definedNames <- definitions
  invisible(wb)
}
