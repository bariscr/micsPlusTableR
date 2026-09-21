col_condition_f <- function(tab_c) {
  
  # --- helpers ---------------------------------------------------------------
  take_last_condition_line <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA_character_)
    
    # normalize newlines and split
    lines <- str_split(gsub("\r\n?", "\n", x), "\n", simplify = FALSE)[[1]]
    lines <- str_trim(lines)
    lines <- lines[nzchar(lines)]
    
    if (!length(lines)) return(NA_character_)
    
    # drop non-condition lines
    keep <- !str_detect(lines, regex("\\.sav\\b", ignore_case = TRUE)) &          # data source
      !str_detect(lines, "^\\s*mutate\\s*\\(") &                            # mutate(...)
      !str_detect(lines, "^\\s*(?:unfilter|filter(?:_block)?)\\s*\\(") &     # filter(...), unfilter(), filter_block(...)
      !str_detect(lines, "^\\s*-\\s*-\\s*-\\s*$") &                         # --- separators
      !str_detect(lines, regex("^\\s*weight\\s*by\\b", ignore_case = TRUE)) # weight by ...
    
    cand <- lines[keep]
    if (!length(cand)) return(NA_character_)
    
    # take the last remaining line (your rule)
    str_trim(tail(cand, 1))
  }
  
  normalize_clause <- function(clause) {
    cl <- str_trim(clause)
    
    # between(VAR, a, b)  --> VAR_a_b
    if (str_detect(cl, regex("^between\\s*\\(", ignore_case = TRUE))) {
      m <- str_match(
        cl,
        regex("between\\s*\\(\\s*([A-Za-z]\\w*)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\)",
              ignore_case = TRUE)
      )
      if (!any(is.na(m))) return(paste0(m[2], "_", m[3], "_", m[4]))
    }
    
    # VAR %in% c(v1, v2, ...)
    if (str_detect(cl, "%in%\\s*c\\s*\\(")) {
      m <- str_match(cl, "([A-Za-z]\\w*)\\s*%in%\\s*c\\s*\\(([^)]*)\\)")
      if (!any(is.na(m))) {
        var <- m[2]
        vals <- str_split(m[3], ",", simplify = FALSE)[[1]] |>
          str_trim() |>
          str_replace_all("^['\"]|['\"]$", "") |>
          str_replace_all("[^A-Za-z0-9._-]", "") |>
          discard(~ .x == "")
        if (length(vals)) return(paste(c(var, vals), collapse = "_"))
      }
    }
    
    # VAR == value
    if (str_detect(cl, "==")) {
      m <- str_match(cl, "([A-Za-z]\\w*)\\s*==\\s*(['\"]?)([^,'\"\\s)]+)\\2")
      if (!any(is.na(m))) {
        var <- m[2]
        val <- if (!is.na(m[4])) m[4] else ""
        val <- str_replace_all(val, "[^A-Za-z0-9._-]", "")
        if (nzchar(val)) return(paste0(var, "_", val))
      }
    }

    # VAR >= value
    if (str_detect(cl, ">=")) {
      m <- str_match(cl, "([A-Za-z]\\w*)\\s*>=\\s*(['\"]?)([^,'\"\\s)]+)\\2")
      if (!any(is.na(m))) {
        var <- m[2]
        val <- if (!is.na(m[4])) m[4] else ""
        val <- str_replace_all(val, "[^A-Za-z0-9._-]", "")
        if (nzchar(val)) return(paste0(var, "_", val))
      }
    }
    
    
    # fallback: sanitize to a token
    fallback <- cl |>
      str_replace_all("\\s+", "_") |>
      str_replace_all("[^A-Za-z0-9._-]", "") |>
      str_remove_all("^_+|_+$")
    if (nzchar(fallback)) fallback else NA_character_
  }
  
  split_clauses <- function(cond) {
    if (is.na(cond) || !nzchar(cond)) return(character())
    str_split(cond, "\\|\\|?|&&?", simplify = FALSE)[[1]] |> str_trim()
  }
  
  # --- build table -----------------------------------------------------------
  out <- tibble()
  
  for (c in seq_along(tab_c$col_index)) {
    raw_cond <- tab_c$col_lgc[c] %||% NA_character_
    cleaned  <- take_last_condition_line(raw_cond)  # <<< key change
    
    clauses <- split_clauses(cleaned)
    tokens  <- map_chr(clauses, normalize_clause) |> discard(is.na)
    
    if (length(tokens) > 1) tokens <- tokens[!duplicated(tokens)]
    
    col_var_name <- if (length(tokens)) {
      nm <- paste(tokens, collapse = "_")
      str_replace_all(nm, "_{2,}", "_")
    } else {
      cleaned %||% ""
      cleaned <- cleaned %||% ""
      cleaned |>
        str_replace_all("[^A-Za-z0-9._-]", "_") |>
        str_replace_all("_+", "_") |>
        str_remove_all("^_+|_+$")
    }
    
    out <- bind_rows(
      out,
      tibble(
        col_index       = tab_c$col_index[c],
        col_condition0  = raw_cond,   # original cell
        col_condition   = cleaned,    # last meaningful condition line
        col_var_name    = col_var_name
      )
    )
  }
  
  out
}
