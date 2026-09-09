write_to_excel <- function(dest, 
                           table, 
                           sheet, 
                           drop_n_unw = FALSE,
                           formatted = FALSE, 
                           zoom_level = 140) {
  
  is_supp <- out_glob$is_supp
  condition_write <- out_glob$condition_write
  tab_direction <- out_glob$tab_direction
  condition_row_index <- out_glob$condition_row_index

  if (isTRUE(formatted) && isTRUE(is_supp)) {
    table$value <- table$value_f
  }

  # If it's output table we also add the conditions to be written
  if (isFALSE(formatted)) {
    table <-
      table |> 
      mutate(value = as.character(value)) |> 
      bind_rows(condition_write)
  }
  
  stopifnot(all(c("row_index", "col_index", "value") %in% names(table)))
  if (!"col_header" %in% names(table)) table$col_header <- ""
  if (!"row_header" %in% names(table)) table$row_header <- ""
  if (!"stat_type"  %in% names(table)) table$stat_type  <- NA_character_
  
wb <- wb_load(dest)
current_sheets <- openxlsx2::wb_get_sheet_names(wb)

if (!sheet %in% current_sheets) {
    if (grepl("_$", sheet)) {
      sheet <- sub("_$", "", sheet)
    } else {
      sheet <- paste0(sheet, "_")
    }
  }
  
  # helpers ---------------------------------------------------------------
  cell_ref     <- function(r, c) wb_dims(rows = r, cols = c)
  apply_numfmt <- function(fmt, dims) if (!is.null(fmt)) wb$add_numfmt(sheet = sheet, dims = dims, numfmt = fmt)
  is_nanish    <- function(x) {
    s <- tolower(trimws(as.character(x)))
    length(s) == 1L && !is.na(s) && identical(s, "nan")
  }
  is_naish     <- function(x) { s <- tolower(trimws(as.character(x))); is.na(x) | s %in% c("na","nan","n/a","n.a.","#n/a") }
  
  # choose fmt by decimals (0 vs 1) and adornment kind
  pick_fmt_by_dec <- function(decimals = 1L, kind = c("base","star","paren")) {
    kind <- match.arg(kind)
    if (decimals == 0L) {
      switch(kind,
             base  = "#,##0",
             star  = "#,##0\"*\"",
             paren = "(#,##0)"
      )
    } else {
      switch(kind,
             base  = "#,##0.0;(#,##0.0)",
             star  = "0.0\"*\";(0.0\"*\")",
             paren = "(#,##0.0)"
      )
    }
  }
  
  # precompute white if you'll use fills
  white <- wb_color(hex = "FFFFFF")
  
  # write loop ------------------------------------------------------------
  for (i in seq_len(nrow(table))) {
    r   <- table$row_index[i]
    c   <- table$col_index[i]
    val <- table$value[i]
    st  <- table$stat_type[i]
    dims <- cell_ref(r, c)
    
    # if stat_type == "n_unw" and we want to drop (formatted mode), blank out and continue
    if (!is.na(st) && st %in% c("n_unw", "n_unw2") && isTRUE(formatted) && tab_direction == "h" && isTRUE(drop_n_unw)) {
      wb$add_data(sheet = sheet, x = "", startRow = r, startCol = c, colNames = FALSE)
      wb$add_fill(sheet = sheet, dims = dims, color = white)
      next
    }
    
    # normalize empties
    if (is.factor(val)) val <- as.character(val)
    if (is_nanish(val)) {
      wb$add_data(sheet = sheet, x = "-", startRow = r, startCol = c, colNames = FALSE)
      next
    }
    if (is_naish(val)) {
      wb$add_data(sheet = sheet, x = "", startRow = r, startCol = c, colNames = FALSE)
      next
    }
    
    # decide decimal places from stat_type: n or n_unw => 0; else => 1
    decimals <- if (!is.na(st) && st %in% c("n","n_unw", "n2", "n_unw2")) 0L else 1L
    
    # parse adornments and numeric
    sval  <- trimws(as.character(val))
    star  <- grepl("\\*$", sval)
    paren <- grepl("^\\s*\\(.*\\)\\s*$", sval)
    num   <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", sval)))
    
    fmt_kind <- if (star) "star" else if (paren) "paren" else "base"
    fmt <- pick_fmt_by_dec(decimals, kind = fmt_kind)
    
    # write ---------------------------------------------------------------
    if (!is.na(num) && (star || paren || grepl("^[0-9, .\\-]+\\)?\\*?$", sval))) {
      wb$add_data(sheet = sheet, x = num, startRow = r, startCol = c, colNames = FALSE)
      apply_numfmt(fmt, dims)
    } else if (is.numeric(val)) {
      wb$add_data(sheet = sheet, x = val, startRow = r, startCol = c, colNames = FALSE)
      apply_numfmt(fmt, dims)
    } else {
      wb$add_data(sheet = sheet, x = sval, startRow = r, startCol = c, colNames = FALSE)
      # leave text unformatted
    }
  }
  
  # view / zoom
  wb$set_sheetview(sheet = sheet, zoom_scale = zoom_level, showGridLines = FALSE)
  
  if (isTRUE(formatted)) {
    # --- extents based on your table (what you just wrote) ---
    n_rows <- max(table$row_index, na.rm = TRUE)
    n_cols <- max(table$col_index, na.rm = TRUE)
    
    # Column 2: blank + white + hide
    wb$add_data(sheet = sheet, x = rep("", n_rows), dims = sprintf("B1:B%d", n_rows), col_names = FALSE)
    wb$add_fill(sheet = sheet, dims = sprintf("B1:B%d", n_rows), color = white)
    wb_set_col_widths(wb, sheet = sheet, cols = 2, widths = 0)
    
    # Specific row (condition_row_index): blank + white + hide
    r <- condition_row_index
    start_col <- 1L
    end_col   <- n_cols
    row_range <- sprintf("%s%d:%s%d", int2col(start_col), r, int2col(end_col), r)
    
    wb$add_data(
      sheet = sheet,
      x = as.data.frame(t(rep("", end_col - start_col + 1L))),
      dims = row_range,
      col_names = FALSE
    )
    wb$add_fill(sheet = sheet, dims = row_range, color = white)
    wb_set_row_heights(wb, sheet = sheet, rows = r, heights = 1) # When set to zero sometimes the bottom border is not visible
  }
  
  # ---- right-align all cells in the table extent (openxlsx2) ----
  r_min <- min(table$row_index, na.rm = TRUE)
  r_max <- max(table$row_index, na.rm = TRUE)
  c_min <- min(table$col_index, na.rm = TRUE)
  c_max <- max(table$col_index, na.rm = TRUE)
  
  dims_tbl <- sprintf("%s%d:%s%d", int2col(c_min), r_min, int2col(c_max), r_max)
  
  openxlsx2::wb_add_cell_style(
    wb, sheet = sheet, dims = dims_tbl,
    apply_alignment = TRUE,
    horizontal = "right", indent = 0, wrap_text = FALSE
  )

  # ---------------------------------

    if (isFALSE(formatted)) {

  wb_set_row_heights(wb, sheet = sheet, rows = condition_row_index, heights = 35)

  # wrap the row
  max_col <- max(table$col_index, na.rm = TRUE)

dims <- sprintf(
  "A%d:%s%d",
  condition_row_index,
  openxlsx2::int2col(max_col),
  condition_row_index
)

wb <- openxlsx2::wb_add_cell_style(
  wb,
  sheet   = sheet,
  dims    = dims,
  wrap_text = TRUE,
  vertical  = "top"
)
    
  }
  # -------------------
  
  
  # Check the consistensies by looking at their rows and if there are issues paint the tab color of the sheet to red
  # 1. Define the list of potential variables to check
  vars_to_check <- c(
    "totals_col_perc_df_issue_n",
    "totals_col_df_issue_n",
    "totals_row_perc_df_issue_n",
    "totals_row_df_issue_n",
    "totals_indent_row_df_issue_n",
    "totals_indent_row_perc_df_issue_n"
  )
  
  # 2. Check if ANY exists AND is > 0
  any_issues_found <- any(sapply(vars_to_check, function(x) {
    exists(x) && get(x) > 0
  }))
  
  any_issues_found <<- any_issues_found

  # 3. Execute logic
#  tab_hex <- if (isTRUE(any_issues_found)) "#FF0000" else "#00FF00"

# Tab color ---------------------------------------------------------------  

#try({
#  wb$set_page_setup(
#    sheet = sheet,
#    orientation = "portrait",
#    tab_color = wb_color(hex = tab_hex)
#  )
#})

clean_sheet_name <- if (grepl("_$", sheet)) sub("_$", "", sheet) else sheet


  if (isTRUE(any_issues_found)) {
    new_sheet <- if (grepl("_$", sheet)) sheet else paste0(clean_sheet_name, "_")
  } else {
    new_sheet <- clean_sheet_name
  }

  if (!identical(new_sheet, sheet) && !(new_sheet %in% current_sheets)) {
    wb <- openxlsx2::wb_set_sheet_names(wb, old = sheet, new = new_sheet)
    
  }

# Change the IDX tab if needed
if (!identical(new_sheet, sheet) && !(new_sheet %in% current_sheets)) {

idx_table_name <-
  out_glob$idx_tab |> 
  filter(table_code == clean_sheet_name) |> 
  mutate(table_code = new_sheet) |> 
  mutate(character = paste0("Table ", table_code, " ", table_name)) |> 
  select(row, col, character)
  
wb$add_data(sheet = "IDX", x = idx_table_name$character, 
            startRow = idx_table_name$row, 
            startCol = idx_table_name$col, colNames = FALSE)

}

  # save once at the end
  wb_save(wb, file = dest, overwrite = TRUE)
}



