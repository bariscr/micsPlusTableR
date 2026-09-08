tabulate_v <- function(skip_row_conditions = FALSE) {
  context <- mics_sheet_context(environment(sys.function()))
  tryCatch({
  mics_scalar_flag(skip_row_conditions, "skip_row_conditions")
  mics_require_plan(environment(sys.function()), "v")

  
  tab <- out_glob$tab
  tab_c <- out_glob$tab_c
  tab_c2 <- out_glob$tab_c2
  tab_r <- out_glob$tab_r
  filter_row <- out_glob$filter_row
  col_header <- out_glob$col_header
  row_header <- out_glob$row_header
  indent_rows <- out_glob$indent_rows
  group_info <- out_glob$group_info
  # ---------- column conditions (clean, no mutate) ----------
  col_condition_table <- col_condition_f(tab_c) %>%
    dplyr::transmute(
      col_index,
      col_logic = col_condition,
      col_condition = dplyr::if_else(
        trimws(col_condition) == "ph",
        "TRUE",
        col_condition
      ),
      col_var_name = dplyr::if_else(
        trimws(col_logic) == "ph",
        ".mics_placeholder_col",
        col_var_name
      )
    )
  
  tab_c2 <- tab_c %>%
    dplyr::left_join(col_condition_table, by = "col_index") %>%
    dplyr::filter(!is.na(col_condition)) %>%
    dplyr::select(-dplyr::any_of(c("col_lgc")))  # drop raw col condition if present
  
  # ---------- row conditions (clean + calculations) ----------
  row_condition_table <- row_condition_f(tab_r)  # must be the hardened version we finalized
  row_condition_eval <- row_condition_table
  placeholder_rows <- trimws(row_condition_eval$row_condition) == "ph"
  row_condition_eval$row_condition[placeholder_rows] <- "TRUE"
  row_condition_eval$row_var_name[placeholder_rows] <- ".mics_placeholder_row"
  row_condition_eval$calculation[placeholder_rows] <- NA_character_
  if (isTRUE(skip_row_conditions)) {
    row_condition_eval$row_condition <- "TRUE"
    row_condition_eval$row_var_name <- ".mics_extra_row"
    row_condition_eval$calculation <- NA_character_
  }
  
  # ---------- base DF, primary filter, then ALL row calc mutates ----------
  df_base    <- filter_row$df[[1]]
  filt1      <- filter_row$filter_condition[1]
  weight_var <- filter_row$weight[[1]]
  
  # helpers
  is_present <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    x1 <- as.character(x[[1]])
    !(is.na(x1) || !nzchar(trimws(x1)))
  }
  apply_chain_df <- function(df_obj, chain) {
    if (!is_present(chain)) return(df_obj)
    df <- df_obj
    eval(parse(text = sprintf("df |> %s", chain)))
  }
  
  # resolve base df (works for character name or object)
  context <- paste0(context, "; source: ", if (is.character(df_base)) df_base else "data frame",
    "; primary filter: ", filt1)
  df0 <- mics_data_source(df_base, environment())
  
  # 1) apply the primary filter (if present)
  df <- if (is_present(filt1)) apply_chain_df(df0, filt1) else df0
  
  # 2) build a single calculation chain from ALL available row calculations
  #    - keep order
  #    - drop NA/empty
  #    - split any pre-joined " |> " chains
  #    - de-duplicate identical steps
  row_calcs <- row_condition_eval$calculation
  row_calcs <- row_calcs[!is.na(row_calcs) & nzchar(trimws(row_calcs))]
  if (length(row_calcs)) {
    steps <- unlist(strsplit(row_calcs, "\\|>", perl = TRUE), use.names = FALSE)
    steps <- trimws(steps)
    steps <- steps[nzchar(steps)]
    steps <- steps[!duplicated(steps)]
    calc_chain_all <- paste(steps, collapse = " |> ")
  } else {
    calc_chain_all <- NA_character_
  }
  
  # 3) apply the full calculation chain (create ALL row vars once)
  context <- paste0(context, "; row calculations: ", calc_chain_all)
  df <- apply_chain_df(df, calc_chain_all)
  base_context <- context
  
  # ---------- helpers used during cell computation ----------
  extract_paren_arg <- function(x, pattern) {
    m <- regexec(pattern, x, ignore.case = TRUE)
    regmatches(x, m)[[1]][2]
  }
  
  make_ind <- function(d, row_condition, row_var_name) {
    if (identical(row_condition, "TRUE")) {
      d %>% dplyr::mutate(!!row_var_name := 1L)
    } else {
      d %>% dplyr::mutate(
        !!row_var_name := dplyr::if_else(!!rlang::parse_expr(row_condition), 1L, 0L)
      )
    }
  }
  
  # ---------- main loop ----------
  out <- tibble::tibble()
  
  for (c in seq_along(tab_c2$col_index)) {
    
    context <- paste0(base_context, "; Excel column ", tab_c2$col_index[c],
      "; column condition: ", tab_c2$col_condition[c])
    col_condition <- tab_c2$col_condition[c]
    
    if (startsWith(trimws(col_condition), "filter(")) {
      col_condition <- sub("^filter\\s*\\((.*)\\)\\s*$", "\\1", col_condition)
    }
    
    
    # make sure df_base is a concrete data.frame, not a delayed tibble reference
    df_work <- as.data.frame(df)
    
    if (!is.na(col_condition) && nzchar(col_condition)) {
      df_c <- dplyr::filter(df_work, !!rlang::parse_expr(col_condition))
    } else {
      df_c <- df_work
    }
    
    
    for (r in seq_along(tab_r$row_index)) {
      st <- tab %>%
        dplyr::filter(row_index == tab_r$row_index[r],
                      col_index == tab_c2$col_index[c]) %>%
        dplyr::pull(stat_type)
      if (length(st) == 0L || is.na(st[[1]])) next
      st <- trimws(st[[1]])
      
      row_condition <- row_condition_eval$row_condition[r]
      row_var_name  <- row_condition_eval$row_var_name[r]
      context <- paste0(base_context, "; Excel row ", tab_r$row_index[r],
        ", column ", tab_c2$col_index[c], "; statistic '", st,
        "'; row condition: ", row_condition, "; column condition: ", col_condition)
      if (st %in% c("n", "n1", "n2", "p", "p1", "p(100)") ||
          grepl("^mean\\s*\\(", st)) mics_weight(df_c, weight_var)
      
      # ---- UNWEIGHTED ----
      
      # If the first row is 100 for n_unw
      if (st %in% c("p_unw(100)",
                    "p(100)",
                    "n_unw(100)",
                    "n(100)"
                    ) && r == 1) {
        value <- 100.0
        
      }
      
      
      
      else if (st %in% c("n_unw", "n_unw1", "n_unw(100)", "n_unw2")) {
        value <- make_ind(df_c, row_condition, row_var_name) %>%
          dplyr::summarise(value = sum(.data[[row_var_name]], na.rm = TRUE)) %>%
          dplyr::pull(value)
       
        
      } else if (st %in% c("p_unw", "p_unw(100)") || startsWith(st, "Mean ")) {
        value <- make_ind(df_c, row_condition, row_var_name) %>%
          dplyr::summarise(value = mean(.data[[row_var_name]], na.rm = TRUE) * 100) %>%
          dplyr::pull(value)
        
      } else if (grepl("^\\s*mean[_ ]?unw\\s*\\(", st, ignore.case = TRUE)) {
        # e.g. mean_unw(CAnum)
        mean_var <- extract_paren_arg(st, "^\\s*mean[_ ]?unw\\s*\\(\\s*([^\\)]+)\\s*\\)")
        df_row <- if (identical(row_condition, "TRUE")) df_c else
          dplyr::filter(df_c, !!rlang::parse_expr(row_condition))
        value <- df_row %>%
          dplyr::summarise(value = mean(.data[[mean_var]], na.rm = TRUE)) %>%
          dplyr::pull(value)
        
        # ---- WEIGHTED ----
      } else if (st %in% c("n", "n1", "n2")) {
        value <- make_ind(df_c, row_condition, row_var_name) %>%
          dplyr::summarise(value = sum(.data[[row_var_name]] * .data[[weight_var]], na.rm = TRUE)) %>%
          dplyr::pull(value)
        
      } else if (st %in% c("p", "p1", "p(100)")) {
        value <- make_ind(df_c, row_condition, row_var_name) %>%
          dplyr::summarise(
            value = (sum(.data[[row_var_name]] * .data[[weight_var]], na.rm = TRUE) /
                       sum(.data[[weight_var]], na.rm = TRUE)) * 100
          ) %>% dplyr::pull(value)
        
        # mean(newvar = <expression>)  (weighted)
      } else if (
        grepl("^\\s*mean\\s*\\(", st, ignore.case = TRUE) &&
        grepl("=", extract_paren_arg(st, "^\\s*mean\\s*\\(\\s*([^\\)]+)\\s*\\)"), fixed = TRUE)
      ) {
        arg <- extract_paren_arg(st, "^\\s*mean\\s*\\(\\s*([^\\)]+)\\s*\\)")
        parts    <- sub("^([^=]+)=\\s*(.*)$", "\\1;;\\2", arg)
        new_name <- trimws(sub(";;.*$", "", parts))
        rhs      <- sub("^.*;;", "", parts)
        
        df_c2 <- df_c %>%
          dplyr::mutate(!!rlang::sym(new_name) := !!rlang::parse_expr(rhs))
        
        df_row <- if (identical(row_condition, "TRUE")) df_c2 else
          dplyr::filter(df_c2, !!rlang::parse_expr(row_condition))
        
        value <- df_row %>%
          dplyr::summarise(
            value = stats::weighted.mean(.data[[new_name]], .data[[weight_var]], na.rm = TRUE)
          ) %>%
          dplyr::pull(value)
        
        # mean_unw (no args): unweighted mean of the row indicator
      } else if (grepl("^\\s*mean[_ ]?unw\\s*$", st, ignore.case = TRUE)) {
        value <- make_ind(df_c, row_condition, row_var_name) %>%
          dplyr::summarise(value = mean(.data[[row_var_name]], na.rm = TRUE) * 100) %>%
          dplyr::pull(value)
        
        # mean(var)  (weighted)
      } else if (grepl("^\\s*mean\\s*\\(", st, ignore.case = TRUE)) {
        mean_var <- extract_paren_arg(st, "^\\s*mean\\s*\\(\\s*([^\\)]+)\\s*\\)")
        df_row <- if (identical(row_condition, "TRUE")) df_c else
          dplyr::filter(df_c, !!rlang::parse_expr(row_condition))
        value <- df_row %>%
          dplyr::summarise(
            value = stats::weighted.mean(.data[[mean_var]], .data[[weight_var]], na.rm = TRUE)
          ) %>%
          dplyr::pull(value)
        
        # ---- OTHER / CONSTANTS ----
      } else if (grepl("^\\s*median\\s*\\(", st, ignore.case = TRUE)) {
        med_var <- extract_paren_arg(st, "^\\s*median\\s*\\(\\s*([^\\)]+)\\s*\\)")
        df_row <- if (identical(row_condition, "TRUE")) df_c else
          dplyr::filter(df_c, !!rlang::parse_expr(row_condition))
        value <- df_row %>%
          dplyr::summarise(value = stats::median(.data[[med_var]], na.rm = TRUE)) %>%
          dplyr::pull(value)
        
      } else if (grepl("^\\s*median[_ ]?unw\\s*\\(", st, ignore.case = TRUE)) {
        median_var <- extract_paren_arg(st, "^\\s*median[_ ]?unw\\s*\\(\\s*([^\\)]+)\\s*\\)")
        df_row <- if (identical(row_condition, "TRUE")) df_c else
          dplyr::filter(df_c, !!rlang::parse_expr(row_condition))
        value <- df_row %>%
          dplyr::summarise(value = median(.data[[median_var]], na.rm = TRUE)) %>%
          dplyr::pull(value)
        
      } else if (st %in% c("100", "100.0")) {
        value <- 100.0
        
      } else {
        stop("Unsupported statistic '", st,
             "' for vertical tabulation. Check the statistic cell; use n, n_unw, p, p_unw, mean(variable), median(variable), or 100.", call. = FALSE)
      }
      
      out <- dplyr::bind_rows(out, tibble::tibble(
        row_index = tab_r$row_index[r],
        col_index = tab_c2$col_index[c],
        row_logic = row_condition_table$row_condition[r],  # cleaned logic (or "TRUE")
        col_logic = tab_c2$col_logic[c],
        stat_type = st,
        value     = value
      ))
    }
  }

out <-  
  out %>%
    dplyr::left_join(col_header, by = c("col_index" = "col_index")) %>%
    dplyr::left_join(row_header, by = c("row_index" = "row_index")) %>%
    dplyr::left_join(indent_rows, by = c("row_index" = "row")) %>%
    dplyr::left_join(group_info, by = c("row_index" = "row")) %>%
    dplyr::mutate(value_f = as.character(value)) |>  
    dplyr::mutate(value_f_view = dplyr::case_when(
      # use thousand separators before changing to character
      stat_type %in% c("n", "n1", "n_unw", "n_unw1", "n2", "n_unw2", "hhmembers") ~ as.character(format(round(value, 0), big.mark = ",")),
      stat_type %in% c("p", "p1", "p_unw", "p(100)", "p_unw(100)", "mean", "mean_unw") ~ as.character(round(value, 1)),
      startsWith(stat_type, "mean") ~ as.character(round(value, 1)),
      TRUE ~ as.character(value)
    ))

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










  
