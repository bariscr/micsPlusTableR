normalize_condition_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, c(
    "[\u201c\u201d]" = "\"",
    "[\u2018\u2019]" = "'",
    "\u2264" = "<=",
    "\u2265" = ">=",
    "\u2260" = "!=",
    "\u2227" = "&",
    "\u2228" = "|"
  ))
  stringr::str_trim(x)
}

row_condition_f <- function(tab_r) {
  split_clauses <- function(cond) {
    stringr::str_split(cond, "\\|\\|?|&&?", simplify = FALSE)[[1]] |>
      stringr::str_trim()
  }
  
  normalize_clause <- function(clause) {
    cl <- stringr::str_trim(clause)
    
    # between(VAR, a, b)  --> VAR_a_b
    if (stringr::str_detect(cl, stringr::regex("^between\\s*\\(", ignore_case = TRUE))) {
      m <- stringr::str_match(
        cl,
        stringr::regex(
          "between\\s*\\(\\s*([A-Za-z]\\w*)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\)",
          ignore_case = TRUE
        )
      )
      if (!any(is.na(m))) return(paste0(m[2], "_", m[3], "_", m[4]))
    }
    
    # VAR %in% c(v1, v2, ...)
    if (stringr::str_detect(cl, "%in%\\s*c\\s*\\(")) {
      m <- stringr::str_match(cl, "([A-Za-z]\\w*)\\s*%in%\\s*c\\s*\\(([^)]*)\\)")
      if (!any(is.na(m))) {
        var  <- m[2]
        vals <- m[3] |>
          stringr::str_split(",", simplify = FALSE) |>
          purrr::pluck(1) |>
          stringr::str_trim() |>
          stringr::str_replace_all("^['\"]|['\"]$", "") |>
          stringr::str_replace_all("[^A-Za-z0-9._-]", "") |>
          purrr::discard(~ .x == "")
        if (length(vals)) return(paste(c(var, vals), collapse = "_"))
      }
    }
    
    # VAR == value
    if (stringr::str_detect(cl, "==")) {
      m <- stringr::str_match(cl, "([A-Za-z]\\w*)\\s*==\\s*(['\"]?)([^,'\"\\s)]+)\\2")
      if (!any(is.na(m))) {
        var <- m[2]
        val <- if (!is.na(m[4])) m[4] else ""
        val <- stringr::str_replace_all(val, "[^A-Za-z0-9._-]", "")
        if (nzchar(val)) return(paste0(var, "_", val))
      }
    }

    # VAR >= value
    if (stringr::str_detect(cl, ">=")) {
      m <- stringr::str_match(cl, "([A-Za-z]\\w*)\\s*>=\\s*(['\"]?)([^,'\"\\s)]+)\\2")
      if (!any(is.na(m))) {
        var <- m[2]
        val <- if (!is.na(m[4])) m[4] else ""
        val <- stringr::str_replace_all(val, "[^A-Za-z0-9._-]", "")
        if (nzchar(val)) return(paste0(var, "_", val))
      }
    }
    
    # fallback
    fallback <- cl |>
      stringr::str_replace_all("\\s+", "_") |>
      stringr::str_replace_all("[^A-Za-z0-9._-]", "") |>
      stringr::str_remove_all("^_+|_+$")
    if (nzchar(fallback)) fallback else NA_character_
  }
  
  row_condition_table <- tibble::tibble()
  
  for (r in seq_along(tab_r$row_index)) {
    raw_cond  <- normalize_condition_text(tab_r$row_lgc[r] %||% "")
    if (is.na(raw_cond)) raw_cond <- ""
    parsed <- mics_parse_row_instruction(raw_cond)
    calc_part <- parsed$calculation
    cleaned <- parsed$condition
    if (is.na(cleaned) || !nzchar(cleaned)) cleaned <- "TRUE"
    
    clauses <- split_clauses(cleaned)
    tokens  <- purrr::map_chr(clauses, normalize_clause) |> purrr::discard(is.na)
    if (length(tokens) > 1) tokens <- tokens[!duplicated(tokens)]
    
    row_var_name <- if (length(tokens)) {
      nm <- paste(tokens, collapse = "_")
      nm <- stringr::str_replace_all(nm, "_{2,}", "_")
      nm
    } else if (identical(cleaned, "TRUE")) {
      "ALL"  # nothing to filter; a friendly sentinel
    } else {
      cleaned |>
        stringr::str_replace_all("[^A-Za-z0-9._-]", "_") |>
        stringr::str_replace_all("_+", "_") |>
        stringr::str_remove_all("^_+|_+$")
    }
    
    row_condition_table <- dplyr::bind_rows(
      row_condition_table,
      tibble::tibble(
        row_index       = tab_r$row_index[r],
        row_condition0  = raw_cond,
        row_condition   = cleaned,      # "TRUE" or cleaned logical
        row_var_name    = row_var_name,
        calculation     = calc_part     # "mutate(... ) |> mutate(...)" or NA
      )
    )
  }
  
  row_condition_table
}
