# The long-format contract adapts the standardized mics-translation cell output,
# omitting duplicate identifiers, values, precision, and denominators.
# The application uses its engine columns internally; conversion happens only
# when Long Format Data is built.

mics_sdmx_long_columns <- function() {
  c(
    "STRUCTURE", "STRUCTURE_ID", "ACTION", "FREQ", "REF_AREA",
    "SURVEY_ID", "TABLE_ID", "TIME_PERIOD",
    "UNIT_MEASURE", "UNIT_MULT", "DECIMALS", "OBS_VALUE", "OBS_STATUS",
    "row_group", "row_label", "row_order",
    "column_group", "column_label", "column_order",
    "statistic", "weighted_n",
    "denominator_n", "suppressible", "stat_type",
    "row", "col", "row_labels", "column",
    "suppress_below", "parenthesize_below", "suppression_basis",
    "value_f", "suppression_status", "suppression_reason", "display_value"
  )
}

mics_require_columns <- function(data, required) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "The cell table is missing required variables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

mics_as_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

mics_clean_text <- function(x, missing = "") {
  x <- as.character(x)
  x[is.na(x)] <- missing
  trimws(x)
}

mics_sdmx_code <- function(x, allow_hyphen = FALSE) {
  x <- toupper(mics_clean_text(x))
  pattern <- if (allow_hyphen) "[^A-Z0-9-]+" else "[^A-Z0-9]+"
  x <- gsub(pattern, "_", x)
  gsub("^_+|_+$", "", x)
}

new_mics_sdmx_config <- function(country,
                                 ref_area,
                                 period,
                                 wave,
                                 frequency = "I",
                                 survey_id = NULL,
                                 time_period = NULL) {
  fields <- list(
    country = country,
    ref_area = ref_area,
    period = period,
    wave = wave,
    frequency = frequency
  )
  invalid <- names(fields)[!vapply(fields, function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }, logical(1))]
  if (length(invalid)) {
    stop("Missing SDMX survey metadata: ", paste(invalid, collapse = ", "),
         call. = FALSE)
  }

  ref_area <- mics_sdmx_code(ref_area)
  period <- trimws(gsub("[()]", "", period))
  time_period <- if (is.null(time_period)) {
    period
  } else {
    trimws(as.character(time_period))
  }
  wave_number <- regmatches(wave, regexpr("[0-9]+", wave))
  wave_code <- if (length(wave_number) && nzchar(wave_number)) {
    paste0("W", wave_number)
  } else {
    mics_sdmx_code(wave)
  }
  if (is.null(survey_id)) {
    survey_id <- paste(ref_area, "MICSPLUS", time_period, wave_code, sep = "_")
  }

  structure(
    list(
      country = trimws(country),
      ref_area = ref_area,
      frequency = trimws(frequency),
      survey_id = mics_sdmx_code(survey_id, allow_hyphen = TRUE),
      time_period = time_period,
      period = period,
      wave = trimws(wave)
    ),
    class = "mics_sdmx_config"
  )
}

validate_mics_sdmx_config <- function(config) {
  required <- c("frequency", "ref_area", "survey_id", "time_period")
  if (!is.list(config)) {
    stop("'config' must be a list containing SDMX survey metadata.", call. = FALSE)
  }
  missing <- setdiff(required, names(config))
  if (length(missing)) {
    stop("Missing SDMX configuration fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invalid <- required[!vapply(config[required], function(x) {
    length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))
  }, logical(1))]
  if (length(invalid)) {
    stop("Invalid SDMX configuration fields: ", paste(invalid, collapse = ", "),
         call. = FALSE)
  }
  invisible(config)
}

mics_public_statistic <- function(stat_type) {
  raw <- trimws(as.character(stat_type))
  lower <- tolower(raw)

  dplyr::case_when(
    grepl("^n(_?unw)?[0-9]*(\\(100\\))?$", lower) |
      lower == "hhmembers" ~ "Count",
    grepl("^p(_?unw)?[0-9]*(\\(100\\))?$", lower) |
      startsWith(lower, "p_sum(") |
      lower %in% c("100", "100.0") |
      grepl("^Mean\\s+", raw) ~ "Percent",
    startsWith(lower, "median") ~ "Median",
    startsWith(lower, "sd") | startsWith(lower, "standard deviation") ~
      "Standard deviation",
    startsWith(lower, "rate") ~ "Rate",
    startsWith(lower, "reference date") ~ "Reference date",
    startsWith(lower, "mean") ~ "Mean",
    !is.na(raw) & nzchar(raw) ~ raw,
    TRUE ~ "Value"
  )
}

mics_internal_statistic <- function(source_stat_type, public_stat_type) {
  raw <- trimws(as.character(source_stat_type))
  lower <- tolower(raw)

  dplyr::case_when(
    grepl("^n_?unw", lower) ~ "scalar_count",
    grepl("^n[0-9]*(\\(100\\))?$", lower) | lower == "hhmembers" ~ "count",
    public_stat_type == "Percent" ~ "distribution",
    public_stat_type == "Median" ~ "median",
    public_stat_type == "Standard deviation" ~ "sd",
    public_stat_type %in% c("Mean", "Rate", "Reference date") ~ "scalar_mean",
    TRUE ~ tolower(mics_sdmx_code(lower))
  )
}

mics_default_unit <- function(stat_type, statistic) {
  dplyr::case_when(
    stat_type == "Percent" ~ "PERCENT",
    stat_type == "Rate" ~ "RATE",
    stat_type == "Reference date" ~ "YEAR",
    stat_type %in% c("Count", "Valid N") ~ "NUMBER",
    stat_type == "Median" ~ "MEDIAN",
    stat_type == "Standard deviation" | statistic == "sd" ~
      "STANDARD_DEVIATION",
    stat_type == "Mean" ~ "MEAN",
    TRUE ~ mics_sdmx_code(stat_type)
  )
}

mics_format_value <- function(value, stat_type, digits) {
  value <- mics_as_numeric(value)
  digits <- as.integer(digits)
  vapply(seq_along(value), function(i) {
    if (is.na(value[[i]])) return("")
    if (stat_type[[i]] %in% c("Count", "Valid N")) {
      if (digits[[i]] > 0L) return(formatC(value[[i]], format = "f",
        digits = digits[[i]], big.mark = " ", decimal.mark = "."))
      return(format(
        round(value[[i]]), big.mark = " ", scientific = FALSE, trim = TRUE
      ))
    }
    if (stat_type[[i]] %in% c(
      "Percent", "Mean", "Median", "Standard deviation", "Rate",
      "Reference date"
    )) {
      return(formatC(value[[i]], format = "f", digits = digits[[i]]))
    }
    as.character(value[[i]])
  }, character(1))
}

mics_column_metadata <- function(table_context, col_index, fallback_label) {
  labels <- mics_clean_text(fallback_label)
  groups <- rep("", length(col_index))
  if (!is.list(table_context)) {
    return(list(label = labels, group = groups))
  }

  leaf <- table_context$header_1
  if (is.data.frame(leaf) && all(c("col_index", "label") %in% names(leaf))) {
    match_index <- match(as.character(col_index), as.character(leaf$col_index))
    matched <- as.character(leaf$label[match_index])
    use <- !is.na(matched) & nzchar(trimws(matched))
    labels[use] <- trimws(matched[use])
  }

  level_names <- paste0("header_", 2:8)
  # Higher levels are outer headers, so assemble them before the lower levels.
  for (level_name in rev(level_names)) {
    ranges <- table_context[[level_name]]
    if (!is.data.frame(ranges) ||
        !all(c("label", "col_start", "col_end") %in% names(ranges))) next
    for (i in seq_along(col_index)) {
      hit <- which(
        !is.na(ranges$col_start) & !is.na(ranges$col_end) &
          col_index[[i]] >= ranges$col_start & col_index[[i]] <= ranges$col_end
      )
      if (!length(hit)) next
      label <- mics_clean_text(ranges$label[hit[[1L]]])
      if (!nzchar(label)) next
      groups[[i]] <- paste(c(groups[[i]], label)[nzchar(c(groups[[i]], label))],
                           collapse = "|")
    }
  }

  list(label = labels, group = groups)
}

mics_legacy_cells_to_standard <- function(cells, table_id, table_context = NULL) {
  mics_require_columns(cells, c("row_index", "col_index", "stat_type", "value"))
  if (is.null(table_id) || length(table_id) != 1L || is.na(table_id) ||
      !nzchar(trimws(table_id))) {
    stop("'table_id' is required when adapting engine cell results.", call. = FALSE)
  }

  n <- nrow(cells)
  get_column <- function(name, default) {
    if (name %in% names(cells)) cells[[name]] else rep(default, n)
  }

  row_index <- mics_as_numeric(cells$row_index)
  col_index <- mics_as_numeric(cells$col_index)
  if (anyNA(row_index) || anyNA(col_index)) {
    stop("Long-format row and column indices cannot be missing.", call. = FALSE)
  }

  cells <- mics_normalize_statistics(cells)
  source_stat_type <- as.character(cells$stat_type)
  stat_type <- mics_public_statistic(source_stat_type)
  statistic <- mics_internal_statistic(source_stat_type, stat_type)
  estimate <- mics_as_numeric(cells$value)
  estimate[!is.finite(estimate)] <- NA_real_

  row_group <- mics_clean_text(get_column("variable_exp", ""))
  row_label <- mics_clean_text(get_column("row_header", ""))
  row_label[!nzchar(row_label)] <- row_group[!nzchar(row_label)]

  headers <- mics_column_metadata(
    table_context,
    col_index,
    get_column("col_header", "")
  )
  column_label <- headers$label
  column_group <- headers$group

  legacy_denominator <- mics_as_numeric(get_column("n_unw", NA_real_))
  legacy_denominator[!is.finite(legacy_denominator)] <- NA_real_
  is_unweighted_count <- grepl("^n_?unw", tolower(source_stat_type))
  is_weighted_count <- grepl("^n[0-9]*(\\(100\\))?$", tolower(source_stat_type))
  unweighted_n <- legacy_denominator
  unweighted_n[is.na(unweighted_n) & is_unweighted_count] <-
    estimate[is.na(unweighted_n) & is_unweighted_count]
  weighted_n <- rep(NA_real_, n)
  weighted_n[is_weighted_count] <- estimate[is_weighted_count]

  suppression_enabled <- if (is.list(table_context) &&
      length(table_context$is_supp) == 1L && !is.na(table_context$is_supp)) {
    isTRUE(table_context$is_supp)
  } else {
    any(!is.na(legacy_denominator))
  }
  suppressible <- rep(suppression_enabled, n) &
    stat_type %in% c("Percent", "Mean", "Median", "Standard deviation", "Rate") &
    !tolower(source_stat_type) %in% c("100", "100.0")

  row_labels <- ifelse(
    row_group == "#Total",
    "#Total",
    ifelse(
      nzchar(row_group) & nzchar(row_label) & row_group != row_label,
      paste(row_group, row_label, sep = "|"),
      ifelse(nzchar(row_label), row_label, row_group)
    )
  )
  column <- ifelse(
    nzchar(column_group), paste(column_group, column_label, sep = "|"),
    column_label
  )

  tibble::tibble(
    table_id = trimws(table_id),
    row_id = paste0("R", formatC(as.integer(row_index), width = 4L, flag = "0")),
    row_group = row_group,
    row_label = row_label,
    row_order = as.integer(row_index),
    column_id = paste0("C", formatC(as.integer(col_index), width = 4L, flag = "0")),
    column_group = column_group,
    column_label = column_label,
    column_order = as.integer(col_index),
    statistic = statistic,
    estimate = estimate,
    weighted_n = weighted_n,
    unweighted_n = unweighted_n,
    denominator_n = unweighted_n,
    suppressible = suppressible,
    digits = ifelse(!is.na(cells$display_digits), cells$display_digits,
                    ifelse(stat_type %in% c("Count", "Valid N", "Reference date"),
                           0L, ifelse(stat_type == "Standard deviation", 2L, 1L))),
    stat_type = stat_type,
    unit_measure = NA_character_,
    row = as.integer(row_index),
    col = as.integer(col_index),
    row_labels = row_labels,
    column = column,
    value = estimate,
    unw = unweighted_n
  )
}

#' Add an SDMX-compatible observation profile to MICS cell results
#'
#' Converts either the package's engine cell results or an already standardized
#' MICS cell table into a 35-variable long-format contract adapted from the
#' mics-translation project. The output omits the custom `table_id`, `value`,
#' `estimate`, `digits`, and `unw` duplicates; use `TABLE_ID`, `OBS_VALUE`,
#' `DECIMALS`, and `denominator_n` instead. Input fields are unchanged.
#' Source coordinates remain in `row_order` and `column_order`; the redundant
#' `row_id`, `column_id`, `ROW_ID`, and `COLUMN_ID` fields are omitted.
#' The output also omits `unweighted_n`, which duplicates `denominator_n` for
#' engine cells. For standardized input, the supplied `denominator_n` is retained.
#' The optional input `unit_measure` override is applied to `UNIT_MEASURE` but
#' is not included as a separate output field.
#'
#' @param cells A data frame with one row per table cell.
#' @param config A list containing `frequency`, `ref_area`, `survey_id`, and
#'   `time_period`.
#' @param table_id Table identifier. Required for raw engine cell results.
#' @param table_context Optional parsed tabulation context used to recover
#'   multi-level column headers and suppression settings.
#' @return A tibble with one row per observation and the 35 standardized fields.
#' @export
as_sdmx_compatible <- function(cells,
                               config,
                               table_id = NULL,
                               table_context = NULL) {
  if (!is.data.frame(cells)) {
    stop("'cells' must be a data frame.", call. = FALSE)
  }
  validate_mics_sdmx_config(config)

  standardized_required <- c(
    "table_id", "row_id", "row_group", "row_label", "row_order",
    "column_id", "column_group", "column_label", "column_order",
    "statistic", "estimate", "weighted_n", "unweighted_n",
    "denominator_n", "suppressible", "digits", "stat_type"
  )
  data <- if (all(standardized_required %in% names(cells))) {
    cells
  } else {
    mics_legacy_cells_to_standard(cells, table_id, table_context)
  }
  mics_require_columns(data, standardized_required)

  n <- nrow(data)
  add_default <- function(name, value) {
    if (!name %in% names(data)) data[[name]] <<- rep(value, n)
  }
  add_default("unit_measure", NA_character_)
  add_default("row", data$row_order)
  add_default("col", data$column_order)
  add_default("row_labels", ifelse(
    data$row_group == "#Total", "#Total",
    paste(data$row_group, data$row_label, sep = "|")
  ))
  add_default("column", ifelse(
    is.na(data$column_group) | data$column_group == "",
    data$column_label,
    paste(data$column_group, data$column_label, sep = "|")
  ))
  add_default("value", data$estimate)
  add_default("unw", data$denominator_n)
  add_default("suppress_below", 25)
  add_default("parenthesize_below", 50)
  add_default("suppression_basis", "unweighted cases")

  data$estimate <- mics_as_numeric(data$estimate)
  data$estimate[!is.finite(data$estimate)] <- NA_real_
  data$value <- data$estimate
  data$weighted_n <- mics_as_numeric(data$weighted_n)
  data$unweighted_n <- mics_as_numeric(data$unweighted_n)
  data$denominator_n <- mics_as_numeric(data$denominator_n)
  data$unw <- data$denominator_n
  data$digits <- as.integer(data$digits)
  data$suppressible <- as.logical(data$suppressible)
  data$suppress_below <- mics_as_numeric(data$suppress_below)
  data$suppress_below[is.na(data$suppress_below)] <- 25
  data$parenthesize_below <- mics_as_numeric(data$parenthesize_below)
  data$parenthesize_below[is.na(data$parenthesize_below)] <- 50
  data$suppression_basis <- mics_clean_text(
    data$suppression_basis, missing = "unweighted cases"
  )
  data$suppression_basis[!nzchar(data$suppression_basis)] <- "unweighted cases"

  unit_override <- as.character(data$unit_measure)
  derived_unit <- mics_default_unit(data$stat_type, data$statistic)
  unit_measure <- ifelse(
    !is.na(unit_override) & nzchar(trimws(unit_override)),
    trimws(unit_override),
    derived_unit
  )
  value_f <- mics_format_value(data$estimate, data$stat_type, data$digits)
  suppression_status <- dplyr::case_when(
    !data$suppressible ~ "publish",
    is.na(data$denominator_n) ~ "missing_denominator",
    data$denominator_n == 0 ~ "zero_denominator",
    data$denominator_n < data$suppress_below ~ "suppress",
    data$denominator_n < data$parenthesize_below ~ "parenthesize",
    TRUE ~ "publish"
  )
  suppression_reason <- dplyr::case_when(
    suppression_status == "missing_denominator" ~
      "Suppression denominator is missing",
    suppression_status == "zero_denominator" ~
      "Zero unweighted cases in denominator",
    suppression_status == "suppress" ~ paste0(
      "Fewer than ", data$suppress_below, " ", data$suppression_basis
    ),
    suppression_status == "parenthesize" ~ paste0(
      data$suppress_below, "-", data$parenthesize_below - 1, " ",
      data$suppression_basis
    ),
    TRUE ~ NA_character_
  )
  display_value <- dplyr::case_when(
    suppression_status == "zero_denominator" ~ "-",
    suppression_status == "suppress" ~ "(*)",
    suppression_status == "parenthesize" ~ paste0("(", value_f, ")"),
    TRUE ~ value_f
  )

  result <- tibble::tibble(
    STRUCTURE = "dataflow",
    STRUCTURE_ID = "MICS_PLUS_TABLE_CELL",
    ACTION = "I",
    FREQ = as.character(config$frequency),
    REF_AREA = as.character(config$ref_area),
    SURVEY_ID = as.character(config$survey_id),
    TABLE_ID = as.character(data$table_id),
    TIME_PERIOD = as.character(config$time_period),
    UNIT_MEASURE = unit_measure,
    UNIT_MULT = 0L,
    DECIMALS = data$digits,
    OBS_VALUE = data$estimate,
    OBS_STATUS = ifelse(is.na(data$estimate), "M", "A"),
    row_group = mics_clean_text(data$row_group),
    row_label = mics_clean_text(data$row_label),
    row_order = as.integer(data$row_order),
    column_group = mics_clean_text(data$column_group),
    column_label = mics_clean_text(data$column_label),
    column_order = as.integer(data$column_order),
    statistic = as.character(data$statistic),
    weighted_n = data$weighted_n,
    denominator_n = data$denominator_n,
    suppressible = data$suppressible,
    stat_type = as.character(data$stat_type),
    row = as.integer(data$row),
    col = as.integer(data$col),
    row_labels = as.character(data$row_labels),
    column = as.character(data$column),
    suppress_below = data$suppress_below,
    parenthesize_below = data$parenthesize_below,
    suppression_basis = data$suppression_basis,
    value_f = value_f,
    suppression_status = suppression_status,
    suppression_reason = suppression_reason,
    display_value = display_value
  )
  result <- result[mics_sdmx_long_columns()]

  validate_sdmx_compatible(result)
  result
}

#' Validate SDMX-compatible MICS long-format data
#'
#' Checks the complete 35-variable contract and the unique observation key:
#' `SURVEY_ID`, `TABLE_ID`, `row_order`, `column_order`, and `TIME_PERIOD`.
#'
#' @param data A data frame produced by [as_sdmx_compatible()].
#' @return `data`, invisibly.
#' @export
validate_sdmx_compatible <- function(data) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.", call. = FALSE)
  mics_require_columns(data, mics_sdmx_long_columns())

  required_fields <- c(
    "STRUCTURE", "STRUCTURE_ID", "ACTION", "FREQ", "REF_AREA",
    "SURVEY_ID", "TABLE_ID", "row_order", "column_order", "TIME_PERIOD",
    "UNIT_MEASURE"
  )
  invalid_fields <- vapply(data[required_fields], function(x) {
    any(is.na(x) | !nzchar(trimws(as.character(x))))
  }, logical(1))
  if (any(invalid_fields)) {
    stop(
      "Missing required observation fields in: ",
      paste(required_fields[invalid_fields], collapse = ", "),
      call. = FALSE
    )
  }

  key <- c("SURVEY_ID", "TABLE_ID", "row_order", "column_order", "TIME_PERIOD")
  if (anyDuplicated(data[key])) {
    stop("Duplicate SDMX-compatible observation keys.", call. = FALSE)
  }
  if (any(is.na(data$suppressible))) {
    stop("'suppressible' cannot contain missing values.", call. = FALSE)
  }
  if (any(data$suppressible & is.na(data$denominator_n))) {
    stop("A suppressible observation has no suppression denominator.",
         call. = FALSE)
  }
  if (any(!data$OBS_STATUS %in% c("A", "M"))) {
    stop("OBS_STATUS must contain only 'A' or 'M'.", call. = FALSE)
  }
  if (any((data$OBS_STATUS == "M") != is.na(data$OBS_VALUE))) {
    stop("OBS_STATUS is inconsistent with OBS_VALUE.", call. = FALSE)
  }
  invisible(data)
}
