# SPSS-style frequency reports. Calculations use codes, not their display labels.
mics_explorer_statistics <- c("Mean", "Median", "Mode", "Std. Deviation", "Range", "Minimum", "Maximum")

mics_explorer_weights <- function(ds, weight_var = NULL) {
  if (is.null(weight_var)) return(rep(1, nrow(ds)))
  if (!weight_var %in% names(ds)) stop("Weight variable not found: ", weight_var)
  x <- haven::zap_missing(ds[[weight_var]])
  if (is.factor(x)) x <- as.character(x)
  w <- suppressWarnings(as.numeric(x))
  if (length(w) && all(is.na(w))) stop("Weight variable '", weight_var, "' is not numeric (or all missing).")
  w[!is.finite(w) | w <= 0] <- 0
  w
}

mics_explorer_number <- function(x, digits = NULL) {
  vapply(x, function(value) {
    if (is.na(value) || !is.finite(value)) return("—")
    d <- if (is.null(digits)) if (abs(value - round(value)) < 1e-8) 0L else 2L else digits
    formatC(value, format = "f", digits = d, big.mark = ",", decimal.mark = ".")
  }, character(1), USE.NAMES = FALSE)
}

mics_explorer_frequency <- function(ds, vars, weight_var = NULL,
                                    statistics = mics_explorer_statistics) {
  if (!length(vars)) return(list())
  unknown <- setdiff(vars, names(ds))
  if (length(unknown)) stop("Variable not found: ", paste(unknown, collapse = ", "))
  weights <- mics_explorer_weights(ds, weight_var)
  lapply(unique(vars), function(variable) {
    x <- ds[[variable]]
    label <- attr(x, "label", exact = TRUE)
    if (is.null(label) || is.na(label) || !nzchar(label)) label <- variable
    labels <- attr(x, "labels", exact = TRUE)
    raw <- if (haven::is.labelled(x)) as.vector(unclass(x)) else x
    missing <- is.na(haven::zap_missing(x))
    if (is.numeric(raw)) missing <- missing | !is.finite(raw)
    included <- weights > 0
    valid <- included & !missing
    total <- sum(weights)
    valid_n <- sum(weights[valid])
    missing_n <- sum(weights[included & missing])
    percent <- function(n, base) if (base > 0) 100 * n / base else rep(NA_real_, length(n))
    categories <- sort(unique(raw[valid]))
    counts <- if (length(categories)) as.numeric(rowsum(weights[valid],
      match(raw[valid], categories), reorder = TRUE)) else numeric()
    display_value <- function(values) {
      text <- as.character(values)
      if (length(labels)) {
        matches <- match(values, unname(labels))
        labelled <- !is.na(matches)
        text[labelled] <- paste0(names(labels)[matches[labelled]], " (", text[labelled], ")")
      }
      text[text == ""] <- "(blank)"
      text
    }
    row <- function(group, value, n, valid_percent = NA_real_, cumulative = NA_real_, kind = "value") {
      data.frame(Group = group, Value = value, Frequency = n,
        Percent = percent(n, total), ValidPercent = valid_percent,
        CumulativePercent = cumulative, kind = kind, stringsAsFactors = FALSE)
    }
    parts <- list()
    if (length(categories)) {
      parts[[length(parts) + 1L]] <- row("Valid", display_value(categories), counts,
        percent(counts, valid_n), percent(cumsum(counts), valid_n))
    }
    parts[[length(parts) + 1L]] <- row("Valid", "Total", valid_n,
      if (valid_n > 0) 100 else NA_real_, kind = "subtotal")
    # Keep SPSS user-missing codes distinct from system-missing values.
    missing_codes <- sort(unique(raw[included & missing & !is.na(raw)]))
    if (length(missing_codes)) {
      counts_missing <- vapply(missing_codes, function(v) sum(weights[included & raw %in% v]), numeric(1))
      parts[[length(parts) + 1L]] <- row("Missing", display_value(missing_codes), counts_missing)
    }
    system_n <- sum(weights[included & is.na(raw)])
    if (system_n > 0) parts[[length(parts) + 1L]] <- row("Missing", "System", system_n)
    parts[[length(parts) + 1L]] <- row("Total", "", total, kind = "total")

    stats <- data.frame(Group = c("N", "N"), Statistic = c("Valid", "Missing"),
                        Value = mics_explorer_number(c(valid_n, missing_n)))
    add_stat <- function(name, value) {
      if (name %in% statistics) stats <<- rbind(stats,
        data.frame(Group = "", Statistic = name, Value = value))
    }
    modes <- categories[counts == max(c(0, counts))]
    mode <- if (length(modes)) as.character(modes[1]) else "—"
    if (is.numeric(raw)) {
      values <- raw[valid]
      w <- weights[valid]
      mean <- if (valid_n > 0) sum(values * w) / valid_n else NA_real_
      median <- NA_real_
      if (length(categories)) {
        cumulative <- cumsum(counts)
        mid <- which(cumulative >= valid_n / 2)[1]
        median <- categories[mid]
        if (abs(cumulative[mid] - valid_n / 2) < 1e-10 && mid < length(categories)) {
          median <- mean(c(categories[mid], categories[mid + 1L]))
        }
      }
      deviation <- if (valid_n > 1) sqrt(sum(w * (values - mean)^2) / (valid_n - 1)) else NA_real_
      min <- if (length(values)) min(values) else NA_real_
      max <- if (length(values)) max(values) else NA_real_
      add_stat("Mean", mics_explorer_number(mean, 2L))
      add_stat("Median", mics_explorer_number(median, 2L))
      add_stat("Mode", mode)
      add_stat("Std. Deviation", mics_explorer_number(deviation, 3L))
      add_stat("Range", mics_explorer_number(max - min))
      add_stat("Minimum", mics_explorer_number(min))
      add_stat("Maximum", mics_explorer_number(max))
    } else add_stat("Mode", mode)
    notes <- character()
    if (length(modes) > 1L && "Mode" %in% statistics) {
      notes <- c(notes, "Multiple modes exist. The smallest value is shown.")
    }
    if (!is.numeric(raw)) notes <- c(notes, "Numeric statistics are not applicable to this variable.")
    if (!is.null(weight_var)) {
      notes <- c(notes, paste0("Weighted by ", weight_var,
        ". Counts are sums of weights; statistics use frequency weights."))
      if (any(!included)) notes <- c(notes, paste(sum(!included), "cases excluded because their weight is missing, nonpositive, or nonfinite."))
    }
    list(variable = variable, label = label, statistics = stats,
         frequencies = do.call(rbind, parts), notes = notes)
  })
}

mics_explorer_table <- function(data, headers, numeric = character(), caption = NULL,
                                groups = FALSE, row_kinds = NULL) {
  tags <- htmltools::tags
  rows <- lapply(seq_len(nrow(data)), function(i) {
    cells <- lapply(seq_along(data), function(j) {
      value <- as.character(data[[j]][i])
      if (is.na(value)) value <- ""
      if (groups && j == 1L) {
        if (value == "") return(NULL)
        if (i > 1L && identical(data[[1]][i], data[[1]][i - 1L])) return(NULL)
        count <- 1L
        while (i + count <= nrow(data) && identical(data[[1]][i], data[[1]][i + count])) count <- count + 1L
        return(tags$th(scope = "rowgroup", rowspan = count, class = "explorer-stub explorer-group", value))
      }
      if (names(data)[j] %in% numeric) tags$td(class = "explorer-number", value)
      else tags$th(scope = "row", class = "explorer-stub",
        colspan = if (groups && j == 2L && data[[1]][i] == "") 2L else NULL, value)
    })
    tags$tr(class = if (!is.null(row_kinds)) paste0("explorer-", row_kinds[i]), cells)
  })
  tags$div(class = "explorer-table-scroll", tabindex = "0", role = "region", `aria-label` = caption,
    tags$table(class = "explorer-table",
      tags$caption(caption),
      if (length(headers)) tags$thead(tags$tr(lapply(seq_along(headers), function(i)
        tags$th(scope = "col", class = if (names(data)[i] %in% numeric) "explorer-number", headers[i])))),
      tags$tbody(rows)))
}

mics_explorer_report_ui <- function(reports, dataset = "hh", weighted = FALSE) {
  tags <- htmltools::tags
  if (!length(reports)) return(tags$div(class = "explorer-empty",
    tags$h3("Explore your data"), tags$p("Select variables and run Data Explorer to see statistics and frequency tables.")))
  tags$div(class = "explorer-report",
    tags$div(class = "explorer-report-heading",
      tags$div(tags$p(class = "explorer-eyebrow", "DATA EXPLORATION"), tags$h2("Frequencies"),
        tags$p(class = "explorer-description", paste(length(reports), if (length(reports) == 1) "variable" else "variables", "·", dataset))),
      tags$span(class = "explorer-badge", if (weighted) "Weighted" else "Unweighted")),
    lapply(reports, function(report) {
      freq <- report$frequencies
      display <- freq[1:6]
      display$Frequency <- mics_explorer_number(freq$Frequency)
      for (column in c("Percent", "ValidPercent", "CumulativePercent")) {
        display[[column]] <- ifelse(is.na(freq[[column]]), "", mics_explorer_number(freq[[column]], 1L))
      }
      tags$section(class = "explorer-variable",
        tags$div(class = "explorer-variable-heading", tags$span(class = "explorer-variable-code", report$variable),
          tags$h3(report$label)),
        tags$div(class = "explorer-tables",
          tags$div(class = "explorer-statistics", mics_explorer_table(report$statistics,
            character(), numeric = "Value", caption = "Statistics", groups = TRUE)),
          tags$div(class = "explorer-frequencies", mics_explorer_table(display,
            c("", "Value", "Frequency", "Percent", "Valid Percent", "Cumulative Percent"),
            numeric = names(display)[3:6], caption = "Frequency table", groups = TRUE, row_kinds = freq$kind))),
        tags$div(class = "explorer-notes",
          tags$p("Percent includes missing cases. Valid Percent and Cumulative Percent use valid cases only."),
          lapply(report$notes, tags$p)))
    }))
}
