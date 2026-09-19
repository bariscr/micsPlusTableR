tabulate_h <- function(skip_row_conditions = FALSE) {
  sheet_context <- mics_sheet_context(environment(sys.function()))
  context <- sheet_context
  tryCatch({
  mics_scalar_flag(skip_row_conditions, "skip_row_conditions")
  mics_require_plan(environment(sys.function()), "h")

  
  tab <- out_glob$tab
  tab_c <- out_glob$tab_c
  tab_c2 <- out_glob$tab_c2
  tab_r <- out_glob$tab_r
  filter_row <- out_glob$filter_row
  col_header <- out_glob$col_header
  row_header <- out_glob$row_header
  indent_rows <- out_glob$indent_rows
  group_info <- out_glob$group_info
  row_logic_lookup <- row_condition_f(tab_r) |>
    dplyr::select(row_index, row_logic = row_condition)
  tab_r_eval <- tab_r
  if (isTRUE(skip_row_conditions)) {
    tab_r_eval$row_lgc <- "TRUE"
  }

  # ---------- helpers ----------
  is_present <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    x1 <- as.character(x[[1]])
    !(is.na(x1) || !nzchar(trimws(x1)))
  }
  
  split_mutate_blocks <- function(xx) {
    # Split on lines that are exactly --- or - - - (allowing extra spaces)
    parts <- unlist(strsplit(
      xx,
      "(?m)^\\s*(?:-\\s-\\s-|---)\\s*$",
      perl = TRUE
    ))
    parts <- trimws(gsub("[\r\n]+", " ", parts))
    parts[nzchar(parts)]
  }
  
  wrap_into_mutate_vec <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    if (!length(x)) return(NA_character_)
    vapply(x, function(xx) {
      # collapse newlines and excess spaces first
      xx <- gsub("[\r\n]+", " ", xx)
      xx <- trimws(xx)
      
      if (!grepl("\\bmutate\\s*\\(", xx) &&
          grepl("^[A-Za-z.][A-Za-z0-9_.]*\\s*=\\s*.+$", xx)) {
        paste0("mutate(", xx, ")")
      } else xx
    }, character(1))
  }
  
  
  
  build_chain <- function(...) {
    parts <- unlist(list(...), recursive = FALSE, use.names = FALSE)
    parts <- as.character(parts)
    parts <- parts[!is.na(parts)]
    parts <- parts[nzchar(trimws(parts))]
    if (!length(parts)) return(NA_character_)
    paste(parts, collapse = " |> ")
  }
  
  normalize_weight <- function(w) {
    if (is.na(w) || !nzchar(trimws(w))) return(NA_character_)
    # remove separators and newlines
    w <- gsub("-+", " ", w)
    w <- gsub("[\r\n]+", " ", w)
    w <- trimws(sub("(?i)^\\s*weight\\s*by\\s*", "", w, perl = TRUE))
    # if multiple words, take the first valid variable-like token
    m <- regexpr("\\b[A-Za-z.][A-Za-z0-9_.]*\\b", w, perl = TRUE)
    if (m[1] != -1)
      substring(w, m[1], m[1] + attr(m, "match.length")[1] - 1)
    else
      NA_character_
  }
  
  resolve_block_weight <- function(weight_cells) {
    vals <- rev(as.character(weight_cells))
    for (s in vals) {
      if (is.na(s) || !nzchar(trimws(s))) next
      s_clean <- gsub("-+", " ", s)
      s_clean <- gsub("[\r\n]+", " ", s_clean)
      nm <- tryCatch(normalize_weight(s_clean), error = function(e) NA_character_)
      if (!is.na(nm) && nzchar(nm)) return(nm)
    }
    NA_character_
  }
  
  
  apply_chain_df <- function(df_obj, chain) {
    if (!is_present(chain)) return(df_obj)
    df <- df_obj
    eval(parse(text = paste0("df |> ", chain)), envir = environment())
  }
  

  
  # ---------- column conditions ----------
  col_condition_table <- col_condition_f(tab_c) %>%
    dplyr::transmute(
      col_index,
      col_logic = stringr::str_replace_all(col_condition, "[\r\n]+", " "),
      col_pred = dplyr::if_else(trimws(col_logic) == "ph", "TRUE", col_logic),
      col_var_name = dplyr::if_else(
        trimws(col_logic) == "ph",
        ".mics_placeholder_col",
        col_var_name
      )
    )
  
  tab_c2 <- tab_c %>%
    dplyr::left_join(col_condition_table, by = "col_index") %>%
    dplyr::filter(!is.na(col_pred)) %>%
    dplyr::select(-dplyr::any_of("col_condition"))
  
  # ---------- mark filter starts ----------
  n_filters <- sum(!is.na(filter_row$filter_condition) &
                     nzchar(trimws(filter_row$filter_condition)))
  
  filter_row <- filter_row %>%
    dplyr::mutate(
      is_start = !is.na(filter_condition) & nzchar(trimws(filter_condition)),
      block    = cumsum(is_start)
    )
  
  # ---------- shared calc ----------
  first_start <- which(filter_row$is_start)[1]
  shared_calc <- if (length(first_start)) {
    wrap_into_mutate_vec(filter_row$calculation[first_start])
  } else NA_character_
  
  # B is the table-wide filter, independent of the first local filter's position.
  global_filter <- filter_row$filter_condition[filter_row$col_index == 2L]
  global_filter <- if (length(global_filter)) global_filter[[1L]] else NA_character_

  # ---------- block processor ----------
  .process_block <- function(b, i_block) {
    local_filter <- if (b$col_start_raw[[1]] == 2L) NA_character_ else b$filt[[1]]
    context <<- paste0(sheet_context,
      ", horizontal block ", i_block, ", starting column ", b$col_start[[1]],
      "; source: ", b$df_base[[1]], "; global filter: ", global_filter,
      "; local filter: ", local_filter, "; calculation: ", b$calc_chain[[1]])
    df_b <- b$df_base[[1]]
    df0 <- mics_data_source(df_b, environment())
    df_global <- apply_chain_df(df0, global_filter)
    calc_chain <- b$calc_chain[[1]]
    wcol <- resolve_block_weight(b$weight_seq[[1]])

    tab_c3 <- tab_c2 |>
      dplyr::filter(col_index >= b$col_start[[1]], col_index < b$col_end_exl[[1]]) |>
      dplyr::mutate(col_condition = col_pred)
    if (!nrow(tab_c3)) return(tibble::tibble())

    # Only entering a mean from a non-mean expires an inherited local filter.
    # A filter starting on a mean applies there; p/count and mean/mean changes
    # preserve scope. Once expired, it stays off until another explicit filter.
    scope <- tab |>
      dplyr::filter(col_index >= b$col_start[[1]], col_index < b$col_end_exl[[1]],
                    !is.na(stat_type)) |>
      dplyr::arrange(row_index, col_index) |>
      dplyr::group_by(row_index) |>
      dplyr::mutate(local_active = {
        statistic <- trimws(stat_type)
        is_mean <- statistic == "mean" | grepl("^mean\\(", statistic) |
          grepl("^Mean\\b", statistic)
        enters_mean <- is_mean & !dplyr::lag(is_mean, default = dplyr::first(is_mean))
        is_present(local_filter) & !dplyr::cumany(enters_mean)
      }) |>
      dplyr::ungroup()

    # Keep the existing filter -> calculation -> cell-statistic order.
    # Prepare each population only once, even when rows have different scopes.
    prepared <- new.env(parent = emptyenv())
    data_for <- function(active) {
      key <- if (active) "local" else "global"
      if (!exists(key, envir = prepared, inherits = FALSE)) {
        data <- if (active) apply_chain_df(df_global, local_filter) else df_global
        data <- apply_chain_df(data, calc_chain)
        mics_weight(data, wcol)
        assign(key, data, envir = prepared)
      }
      get(key, envir = prepared, inherits = FALSE)
    }

    purrr::map_dfr(seq_len(nrow(tab_r_eval)), function(r) {
      row_scope <- scope[scope$row_index == tab_r_eval$row_index[r], ]
      active <- row_scope$local_active[match(tab_c3$col_index, row_scope$col_index)]
      active[is.na(active)] <- FALSE
      purrr::map_dfr(unique(active), function(use_local) {
        calc_cells(
          df = data_for(use_local),
          tab_r = tab_r_eval[r, , drop = FALSE],
          tab_c3 = tab_c3[active == use_local, , drop = FALSE],
          tab = tab,
          weight_var = wcol,
          weighted = TRUE
        ) |>
          dplyr::mutate(filt1 = global_filter,
                        filt2 = if (use_local) local_filter else NA_character_)
      }) |>
        dplyr::arrange(col_index)
    }) |>
      dplyr::mutate(
        cond = i_block,
        df = if (is.character(df_b)) df_b else deparse(substitute(df_b)),
        calc_chain = calc_chain,
        weight_var = wcol,
        col_start = b$col_start[[1]],
        col_end_excl = b$col_end_exl[[1]]
      )
  }

  # Source, weight and calculation inheritance retain their filter-block
  # windows. Only an actual filter starts a new block; a mutate-only cell
  # cannot cut off the remaining columns.
  build_blocks <- function(fr) {
    fr %>%
      dplyr::group_by(block) %>%
      dplyr::summarise(
        df_base = dplyr::first(df),
        weight_seq = list(weight),
        filt = dplyr::first(filter_condition[!is.na(filter_condition) & nzchar(trimws(filter_condition))]),
        calc_chain = {
          calc_clean <- wrap_into_mutate_vec(calculation)
          build_chain(shared_calc, calc_clean)
        },
        col_start_raw = dplyr::first(col_index),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        col_start = dplyr::if_else(col_start_raw == 2L, 3L, col_start_raw),
        col_end_exl = dplyr::lead(col_start_raw),
        col_end_exl = dplyr::if_else(is.na(col_end_exl), Inf, col_end_exl)
      )
  }

  if (n_filters == 0L) {
    stop("No usable filter block was found. Add a filter(...) entry beside the .sav data source in the worksheet condition row.")
  }
  blocks <- build_blocks(filter_row)
  cell_results_all <- purrr::map_dfr(seq_len(nrow(blocks)),
    ~ .process_block(blocks[.x, , drop = FALSE], .x))

  # ---------- output ----------
  results <- cell_results_all %>%
    dplyr::mutate(col_end = col_end_excl - 1) %>%
    dplyr::left_join(col_header, by = c("col_index" = "col_index")) %>%
    dplyr::left_join(row_header, by = c("row_index" = "row_index")) |> 
    dplyr::left_join(indent_rows, by = c("row_index" = "row")) |> 
    dplyr::left_join(group_info, by = c("row_index" = "row")) 

  if (isTRUE(skip_row_conditions)) {
    results <- results |>
      dplyr::select(-row_logic) |>
      dplyr::left_join(row_logic_lookup, by = "row_index")
  }
 
# How many ns in columns
count_col_ns <- tab |> distinct(col_index, stat_type) |> filter(stat_type == "n") |> nrow()

# If there are multiple blocks, and as many ns, then we use the respective n_unw for each block
  if (filter_row |> nrow() > 1 && count_col_ns == filter_row |> nrow()) {
  
  df_n_unw <- results %>%
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = col_start,
                  col_end_1   = col_end)

# If there are multiple blocks, but only one n, then we use the only n_unw for all table
  } else if (filter_row |> nrow() > 1 && count_col_ns == 1) {

    df_n_unw <- results %>%
    dplyr::mutate(min_col_start = min(col_start)) |> 
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = min_col_start,
                  col_end_1   = col_end)
  
# Other conditions, to be specified in a new case
  } else {

  df_n_unw <- results %>%
    dplyr::filter(stat_type == "n_unw") %>%
    dplyr::select(row_index, n_unw = value,
                  col_start_1 = col_start,
                  col_end_1   = col_end)

  }

  
  results1 <- results %>%
    dplyr::left_join(
      df_n_unw,
      by = dplyr::join_by(
        row_index == row_index,
        dplyr::between(col_index, col_start_1, col_end_1)
      )
    ) |>
    restore_suppression_basis()
  
  out <-
    results1 |> 
    dplyr::mutate(
      value_f_org = dplyr::case_when(
        is.nan(value) ~ "-",
        stat_type %in% c("n", "n1", "n_unw", "n2", "n_unw2", "hhmembers") ~ as.character(format(round(value, 0), big.mark = ",")),
        stat_type %in% c("p", "n1", "p_unw", "p(100)", "p_unw(100)", "mean", "mean_unw") ~ as.character(round(value, 1)),
        TRUE ~ as.character(value)
      ),
      value_f_org = format_mean_display(value, stat_type, value_f_org)
    ) 


if (out_glob$is_supp) {
  out <- out |> 
    dplyr::mutate(
      value_f = dplyr::case_when(
        (is.na(n_unw) | n_unw == 0) & stat_type == "100" ~ "0",
        stat_type %in% c("n", "n1", "n_unw", "100") ~ as.character(value),
        (is.na(n_unw) | n_unw == 0) & stat_type != "100" ~ "-",
        n_unw < 25                            ~ "(*)",
        dplyr::between(n_unw, 25, 49)         ~ paste0("(", round(value, 1), ")"),
        n_unw >= 50                           ~ as.character(value),
        TRUE                                  ~ as.character(value)
      )
    ) |> 
       dplyr::mutate(
      value_f_view = dplyr::case_when(
        stat_type %in% c("n", "n1", "n_unw") ~ value_f_org,
        (is.na(n_unw) | n_unw == 0) & stat_type == "100" ~ "0",
        (is.na(n_unw) | n_unw == 0) & stat_type != "100" ~ "-",
        n_unw < 25                            ~ "(*)",
        dplyr::between(n_unw, 25, 49)         ~ paste0("(", value_f_org, ")"),
        n_unw >= 50                           ~ value_f_org,
        TRUE                                  ~ value_f_org
      )
    ) 

  } else {
    out <- out |> 
      dplyr::mutate(
        value_f = value_f_org,
        value_f_view = value_f_org
      )
  }

  # Add the variable names to the output
  out <-
  out %>%
  mutate(var_name_row = purrr::map_chr(row_logic, extract_var)) 
  
  out <-
    out %>%
    mutate(var_name_col = purrr::map_chr(col_logic, extract_var)) 

return(out)
  


  }, error = function(e) mics_abort_context(e, context))
}
