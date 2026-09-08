
## Stat function ----

calc_cells <- function(df, tab_r, tab_c3, tab, weight_var, weighted = FALSE) {
  context <- mics_sheet_context(environment(sys.function()))
  tryCatch({
  mics_require_columns(df, character(), "df")
  mics_require_columns(tab_r, c("row_index", "row_lgc"), "tab_r")
  mics_require_columns(tab_c3, c("col_index", "col_condition", "col_var_name"), "tab_c3")
  mics_require_columns(tab, c("row_index", "col_index", "stat_type"), "tab")
  if (!(identical(weighted, TRUE) || identical(weighted, FALSE) || identical(weighted, "both"))) {
    stop("'weighted' must be TRUE, FALSE, or 'both'.", call. = FALSE)
  }
  if (isTRUE(weighted)) mics_weight(df, weight_var)

  # normalize to "unweighted" / "weighted" / "both"
  weight_mode <- if (isTRUE(weighted)) "weighted" else
    if (identical(weighted, "both")) "both" else "unweighted"
  
  cell_results <- tibble()
  
  for (r in seq_along(tab_r$row_index)) {
    context <- paste0(mics_sheet_context(environment(sys.function())),
      "; Excel row ", tab_r$row_index[r], "; row condition: ", tab_r$row_lgc[r])
    row_condition <- tab_r$row_lgc[r]
    row_condition_eval <- normalize_condition_text(row_condition)
    if (!is.na(row_condition_eval) && identical(trimws(row_condition_eval), "ph")) {
      row_condition_eval <- "TRUE"
    }
    
    # filter by row condition (safe tidy-eval)
    filtered_data <- if (is.na(row_condition_eval) ||
                         !nzchar(row_condition_eval) ||
                         identical(row_condition_eval, "TRUE")) {
      df
    } else {
      dplyr::filter(df, !!parse_expr(row_condition_eval))
    }
    
    for (c in seq_along(tab_c3$col_index)) {
      context <- paste0(mics_sheet_context(environment(sys.function())),
        "; Excel row ", tab_r$row_index[r], ", column ", tab_c3$col_index[c],
        "; row condition: ", tab_r$row_lgc[r],
        "; column condition: ", tab_c3$col_condition[c])
      # stat type for this cell
      stat_type <- tab %>%
        dplyr::filter(row_index == tab_r$row_index[r],
                      col_index == tab_c3$col_index[c]) %>%
        dplyr::pull(stat_type)
      stat_type <- if (length(stat_type)) stat_type[[1]] else NA_character_
      
      context <- paste0(context, "; statistic: ", stat_type)
      value <- NA_real_
      
      if (!is.na(stat_type)) {
        
        # ---------- n (count of TRUE) ----------
        if (stat_type %in% c("n", "n1")) {
          col_condition <- tab_c3$col_condition[c]
          col_var_name  <- tab_c3$col_var_name[c]
          
          tmp <- filtered_data %>%
            dplyr::mutate(
              !!col_var_name := dplyr::case_when(
                (!!parse_expr(col_condition)) ~ 1L,
                TRUE ~ 0L
              )
            )
          
          if (weight_mode %in% c("unweighted","both")) {
            value <- tmp %>%
              dplyr::summarise(value = sum(.data[[col_var_name]], na.rm = TRUE)) %>%
              dplyr::pull(value)
          } else if (weight_mode == "weighted") {
            value <- tmp %>%
              dplyr::summarise(
                value = sum(.data[[col_var_name]] * .data[[weight_var]], na.rm = TRUE)
              ) %>% dplyr::pull(value)
          }
          
          
          # ---------- n_unw (count of TRUE) ----------
          
          # if its member weight ------
        } else if (stat_type %in% c("n_unw", "n_unw1") & tab_c3$col_condition[c] == "hhmembers") {
          col_condition <- "total == 1"
          col_var_name  <- "total_1"
          
          tmp <- filtered_data %>%
            dplyr::mutate(
              !!col_var_name := dplyr::case_when(
                (!!parse_expr(col_condition)) ~ 1L,
                TRUE ~ 0L
              )
            )
          
          #if (weight_mode %in% c("unweighted","both")) {
          value <- tmp %>%
            dplyr::summarise(value = sum(.data[[col_var_name]] * .data[["HLnum"]], na.rm = TRUE)) %>%
            dplyr::pull(value)
          
          
          
          # ---------- "Mean " (binary % * 100) ----------
        } else if (stat_type %in% c("n_unw", "n_unw1") & tab_c3$col_condition[c] != "hhmembers") {
          col_condition <- tab_c3$col_condition[c]
          col_var_name  <- tab_c3$col_var_name[c]
          
          tmp <- filtered_data %>%
            dplyr::mutate(
              !!col_var_name := dplyr::case_when(
                (!!parse_expr(col_condition)) ~ 1L,
                TRUE ~ 0L
              )
            )
          
          #if (weight_mode %in% c("unweighted","both")) {
          value <- tmp %>%
            dplyr::summarise(value = sum(.data[[col_var_name]], na.rm = TRUE)) %>%
            dplyr::pull(value)
          
          
          
          # ---------- "Mean " (binary % * 100) ----------
        } else if (grepl("^Mean\\b", stat_type)) {
          col_condition <- tab_c3$col_condition[c]
          col_var_name  <- tab_c3$col_var_name[c]
          
          tmp <- filtered_data %>%
            dplyr::mutate(
              !!col_var_name := dplyr::case_when(
                (!!parse_expr(col_condition)) ~ 1L,
                TRUE ~ 0L
              )
            )
          
          # original used unweighted mean * 100
          value <- tmp %>%
            dplyr::summarise(value = mean(.data[[col_var_name]], na.rm = TRUE) * 100) %>%
            dplyr::pull(value)
          
          # ---------- mean(x) ----------
        } else if (grepl("^mean\\(", stat_type)) {
          col_condition <- tab_c3$col_condition[c]
          mean_var <- sub(".*\\(([^)]*)\\).*", "\\1", stat_type)
          
          tmp <- filtered_data %>%
            dplyr::filter( !!parse_expr(col_condition) )
          
          if (weight_mode %in% c("unweighted","both")) {
            value <- tmp %>%
              dplyr::summarise(value = mean(.data[[mean_var]], na.rm = TRUE)) %>%
              dplyr::pull(value)
          } else if (weight_mode == "weighted") {
            value <- tmp %>%
              dplyr::summarise(
                value = stats::weighted.mean(.data[[mean_var]], .data[[weight_var]], na.rm = TRUE)
              ) %>% dplyr::pull(value)
          }
          
          # ---------- median(x) ----------
        } else if (grepl("^median\\(", stat_type)) {
          col_condition <- tab_c3$col_condition[c]
          med_var <- sub(".*\\(([^)]*)\\).*", "\\1", stat_type)
          
          tmp <- filtered_data %>% dplyr::filter( !!parse_expr(col_condition) )
          
          value <- tmp %>%
            dplyr::summarise(value = stats::median(.data[[med_var]], na.rm = TRUE)) %>%
            dplyr::pull(value)
          
          # ---------- p (proportion * 100) ----------
        } else if (stat_type %in% c("p", "p1", "mean")) {
          col_condition <- tab_c3$col_condition[c]
          col_var_name  <- tab_c3$col_var_name[c]
          
          tmp <- filtered_data %>%
            dplyr::mutate(
              !!col_var_name := dplyr::case_when(
                (!!parse_expr(col_condition)) ~ 1L,
                TRUE ~ 0L
              )
            )
          
          if (weight_mode %in% c("unweighted","both")) {
            value <- tmp %>%
              dplyr::summarise(value = sum(.data[[col_var_name]], na.rm = TRUE) / dplyr::n() * 100) %>%
              dplyr::pull(value)
          } else if (weight_mode == "weighted") {
            value <- tmp %>%
              dplyr::summarise(
                value = sum(.data[[col_var_name]] * .data[[weight_var]], na.rm = TRUE) /
                  sum(.data[[weight_var]], na.rm = TRUE) * 100
              ) %>% dplyr::pull(value)
          } else if (weight_mode == "weighted") {
            value <- tmp %>%
              dplyr::summarise(
                value = sum(.data[[col_var_name]] * .data[[weight_var]], na.rm = TRUE) /
                  sum(.data[[weight_var]], na.rm = TRUE) * 100
              ) %>% dplyr::pull(value)
          }
        } else if (grepl("^p_sum\\(", stat_type)) {
          col_condition <- tab_c3$col_condition[c]
          mics_weight(filtered_data, weight_var)
          p_sum_var <- sub(".*\\(([^)]*)\\).*", "\\1", stat_type)
          
          tmp <- filtered_data %>%
            dplyr::filter( !!parse_expr(col_condition) )
          
            value <- tmp %>%
              dplyr::summarise(
                value = sum(.data[[p_sum_var]], na.rm = TRUE) /
                  sum(.data[[weight_var]], na.rm = TRUE) * 100
              ) %>% dplyr::pull(value)
            
          } else if (stat_type %in% c("100","100.0")) {
          value <- 100.0
        } else {
          stop("Unsupported statistic '", stat_type,
               "' for horizontal cell calculation. Use n, n_unw, p, mean(variable), median(variable), p_sum(variable), or 100.", call. = FALSE)
        }
      } 
      
      # append result row
      cell_results <- dplyr::bind_rows(
        cell_results,
        tibble(
          row_index = tab_r$row_index[r],
          col_index = tab_c3$col_index[c],
          row_logic = tab_r$row_lgc[r],
          col_logic = if ("col_logic" %in% names(tab_c3)) {
            tab_c3$col_logic[c]
          } else {
            tab_c3$col_condition[c]
          },
          stat_type = stat_type,
          value     = value
        )
      )
    }
  }
  
  cell_results

  }, error = function(e) mics_abort_context(e, context))
}

