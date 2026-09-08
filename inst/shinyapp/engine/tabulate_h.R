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
  
  # ---------- block processor ----------
  .process_block <- function(b, i_block) {
    context <<- paste0(sheet_context,
      ", horizontal block ", i_block, ", starting column ", b$col_start[[1]],
      "; source: ", b$df_base[[1]], "; filter: ", b$filt[[1]],
      "; calculation: ", b$calc_chain[[1]])
    df_b <- b$df_base[[1]]
    df0  <- mics_data_source(df_b, environment())
    
    filt_chain <- b$filt[[1]]
    calc_chain <- b$calc_chain[[1]]
    
    df <- apply_chain_df(df0, filt_chain)
    df <- apply_chain_df(df,  calc_chain)
    
    # Determine latest valid weight name after all chains
    wcol <- resolve_block_weight(b$weight_seq[[1]])
    mics_weight(df, wcol)
    
    tab_c3 <- tab_c2 |>
      dplyr::filter(col_index >= b$col_start[[1]], col_index < b$col_end_exl[[1]]) |>
      dplyr::mutate(col_condition = col_pred)
    
    calc_cells(
      df         = df,
      tab_r      = tab_r_eval,
      tab_c3     = tab_c3,
      tab        = tab,
      weight_var = wcol,
      weighted   = TRUE
    ) |>
      dplyr::mutate(
        cond         = i_block,
        df           = if (is.character(df_b)) df_b else deparse(substitute(df_b)),
        filt1        = if (is_present(filt_chain)) filt_chain else NA_character_,
        calc_chain   = if (is_present(calc_chain)) calc_chain else NA_character_,
        weight_var   = wcol,
        col_start    = b$col_start[[1]],
        col_end_excl = b$col_end_exl[[1]]
      )
  }
  
  # ---------- build blocks ----------
  build_blocks <- function(fr) {
    fr %>%
      dplyr::group_by(block) %>%
      dplyr::summarise(
        df_base    = dplyr::first(df),
        weight_seq = list(weight),          # capture all weight cells in window
        filt       = dplyr::first(filter_condition[!is.na(filter_condition) & nzchar(trimws(filter_condition))]),
        calc_chain = {
          calc_clean <- wrap_into_mutate_vec(calculation)
          build_chain(shared_calc, calc_clean)
        },
        col_start_raw = dplyr::first(col_index),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        col_start   = dplyr::if_else(block == min(block), col_start_raw + 1L, col_start_raw),
        col_end_exl = dplyr::lead(col_start_raw),
        col_end_exl = dplyr::if_else(is.na(col_end_exl), Inf, col_end_exl)
      )
  }
  
  # ---------- cases ----------
  if (n_filters == 1) {
    
    blocks <- build_blocks(filter_row)
    cell_results_all <- purrr::map_dfr(seq_len(nrow(blocks)),
                                       ~ .process_block(blocks[.x, , drop = FALSE], .x))
    
  } else if (n_filters >= 2 &&
             (filter_row$col_index[which(filter_row$is_start)[2]] -
              filter_row$col_index[which(filter_row$is_start)[1]] == 1)) {
    
    start_rows  <- which(filter_row$is_start)
    next_starts <- dplyr::lead(filter_row$col_index)
    next_starts[is.na(next_starts)] <- Inf
    
    cell_results_all <- purrr::map_dfr(seq_along(start_rows), function(i_block) {
      
      cond    <- start_rows[i_block]
      context <<- paste0(sheet_context, ", horizontal block ", i_block,
        ", starting column ", filter_row$col_index[cond],
        "; source: ", filter_row$df[[cond]],
        "; filter: ", filter_row$filter_condition[cond],
        "; calculation: ", filter_row$calculation[cond])
      df_base <- filter_row$df[[cond]]
      df0     <- mics_data_source(df_base, environment())
      
      # universal + adjacent filters
      filt1      <- filter_row$filter_condition[start_rows[1]]
      filt2      <- if (i_block > 1) filter_row$filter_condition[cond] else NA_character_
      filt_chain <- build_chain(filt1, filt2)
      
      # column window
      col_start  <- filter_row$col_index[cond]
      col_end_ex <- next_starts[cond]
      
      rows_in_block <- which(filter_row$row_index == filter_row$row_index[cond] &
                               filter_row$col_index >= col_start &
                               filter_row$col_index <  col_end_ex)
      calc_vec   <- wrap_into_mutate_vec(filter_row$calculation[rows_in_block])
      calc_chain <- build_chain(shared_calc, calc_vec)
      
      df <- apply_chain_df(df0, filt_chain)
      df <- apply_chain_df(df,  calc_chain)
      
      # detect weight across all cells in this window
      wcol <- resolve_block_weight(filter_row$weight[rows_in_block])
      mics_weight(df, wcol)
      
      tab_c3 <- tab_c2 |>
        dplyr::filter(col_index >= col_start, col_index < col_end_ex) |>
        dplyr::mutate(col_condition = col_pred)
      
      calc_cells(
        df         = df,
        tab_r      = tab_r_eval,
        tab_c3     = tab_c3,
        tab        = tab,
        weight_var = wcol,
        weighted   = TRUE
      ) |>
        dplyr::mutate(
          cond         = i_block,
          df           = if (is.character(df_base)) df_base else deparse(substitute(df_base)),
          filt1        = filt1,
          filt2        = filt2,
          calc_chain   = calc_chain,
          weight_var   = wcol,
          col_start    = col_start,
          col_end_excl = col_end_ex
        )
    })
    
  } else if (n_filters >= 2 &&
             (filter_row$col_index[which(filter_row$is_start)[2]] -
              filter_row$col_index[which(filter_row$is_start)[1]] > 1)) {
    
    blocks <- build_blocks(filter_row)
    cell_results_all <- purrr::map_dfr(seq_len(nrow(blocks)),
                                       ~ .process_block(blocks[.x, , drop = FALSE], .x))
    
  } else {
    stop("No usable filter block was found. Add a filter(...) entry beside the .sav data source in the worksheet condition row.")
  }
  
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
    ) 
  
  out <-
    results1 |> 
    dplyr::mutate(
      value_f_org = dplyr::case_when(
        stat_type %in% c("n", "n1", "n_unw", "n2", "n_unw2", "hhmembers") ~ as.character(format(round(value, 0), big.mark = ",")),
        stat_type %in% c("p", "n1", "p_unw", "p(100)", "p_unw(100)", "mean", "mean_unw") ~ as.character(round(value, 1)),
        TRUE ~ as.character(value)
      )
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
