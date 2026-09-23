# R/write_footnotes.R

#' Write footnotes below the tabulation based on table contents
#'
#' Scans a table for special markers and writes the corresponding footnotes
#' to the target Excel sheet.
#'
#' Always draws borders:
#' - Bottom border: under the last footnote row (if any), otherwise under last table row
#' - Right border: on the last column (from top of table to the bottom-border row)
#'
#' @param dest Path of the Excel workbook to write to.
#' @param sheet Sheet name or index.
#' @param df A data frame/tibble representing the table whose values will be scanned for markers.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#' @importFrom stringr str_detect
#' @importFrom openxlsx2 wb_load wb_save wb_add_font wb_add_border int2col
write_footnotes <- function(dest = dest_f,
                            sheet,
                            df = cell_results) {
  
wb <- openxlsx2::wb_load(dest)
current_sheets <- openxlsx2::wb_get_sheet_names(wb)



if (!sheet %in% current_sheets) {
    if (grepl("_$", sheet)) {
      sheet <- sub("_$", "", sheet)
    } else {
      sheet <- paste0(sheet, "_")
    }
  }



  tab <- out_glob$tab
  tab_org <- out_glob$tab_org
  a_cells <- out_glob$a_cells


  if (missing(sheet) || is.null(sheet)) {
    sheet <- base::get0("sheet",
                        envir = parent.frame(),
                        inherits = TRUE,
                        ifnotfound = NULL)
    if (is.null(sheet)) {
      stop("Argument 'sheet' is missing and no global/caller 'sheet' was found.")
    }
  }
  

  if (is.null(tab_org)) {
    stop("Object 'tab_org' was not found (needed to place border/footnotes).")
  }
  

  if (is.null(tab)) {
    stop("Object 'tab' was not found (needed to find last table column).")
  }
  
  # Only eligible tables use the suppression display in the formatted export.
  vec <- if (isTRUE(out_glob$is_supp)) trimws(df$value_f) else character()
  
  is_1 <- any(stringr::str_detect(vec, "^\\([^*]+\\)$"), na.rm = TRUE)
  is_2 <- any(vec == "(*)", na.rm = TRUE)
  is_3 <- any(vec == "-", na.rm = TRUE)
  
  foot1 <- "( ) Figures that are based on 25-49 unweighted cases"
  foot2 <- "(*) Figures that are based on fewer than 25 unweighted cases"
  foot3 <- "- denotes 0 unweighted cases in the denominator"
  
  txt <- NULL
  if (is_1 && is_2 && is_3) {
    txt <- c(foot1, foot2, foot3)
  } else if (is_1 && is_2 && !is_3) {
    txt <- c(foot1, foot2)
  } else if (is_1 && !is_2 && is_3) {
    txt <- c(foot1, foot3)
  } else if (!is_1 && is_2 && is_3) {
    txt <- c(foot2, foot3)
  } else if (is_1 && !is_2 && !is_3) {
    txt <- c(foot1)
  } else if (!is_1 && is_2 && !is_3) {
    txt <- c(foot2)
  } else if (!is_1 && !is_2 && is_3) {
    txt <- c(foot3)
  }
  
  wb <- openxlsx2::wb_load(dest)
  
  # ---- bounds (columns from tab; rows from tab_org) ----
  last_table_row  <- max(tab$row_index, na.rm = T) + 1
  first_table_row <- min(tab$row_index, na.rm = TRUE)
  
  last_col_num    <- (a_cells |> filter(character == "IDX") |> pull(col)) - 1
  last_col_let    <- openxlsx2::int2col(last_col_num)
  # Keep the spacer width here because this code already locates the column
  # before IDX and uses its left edge for the table and footnote right border.
  wb$set_col_widths(sheet = sheet, cols = last_col_num, widths = 0.64)
  # Use the exact width saved by Excel for a displayed 0.64 in the MICS
  # template; set_col_widths() otherwise adds font-dependent padding.
  worksheet <- wb$worksheets[[match(sheet, current_sheets)]]
  columns <- worksheet$unfold_cols()
  spacer <- as.integer(columns$min) == last_col_num
  columns$width[spacer] <- "1.1640625"
  columns$bestFit[spacer] <- ""
  worksheet$fold_cols(columns)
  
  last_col_num2 <- last_col_num - 1
  last_col_let2    <- openxlsx2::int2col(last_col_num2)
  
  # default: bottom border under last table row
  border_row <- last_table_row
  
  # ---- write footnotes if any ----
  if (!is.null(txt) && length(txt) > 0) {
    
    start_row_foot_df <- last_table_row 
    end_row_foot_df   <- start_row_foot_df + length(txt) - 1
    
    start_row_foot <- last_table_row + 1
    end_row_foot   <- start_row_foot + length(txt) - 1
    
    wb$add_data(
      sheet = sheet,
      x     = txt,
      dims  = paste0("A", start_row_foot_df)
    )
    
    footnote_dims <- paste0("A", start_row_foot_df, ":A", end_row_foot_df)
    # Style only the newly written conditional notes; preserve existing notes' wrapping.
    wb$add_font(
      sheet = sheet,
      dims  = footnote_dims,
      name  = "Arial",
      size  = 8
    )
    wb$add_cell_style(
      sheet = sheet,
      dims = footnote_dims,
      wrap_text = FALSE
    )
    
    border_row <- end_row_foot
    
  }
  
  # ---- bottom border across to the last column ----
  top_dims <- paste0("A", border_row, ":", last_col_let2, border_row)
  wb$add_border(
    sheet = sheet,
    dims  = top_dims,
    bottom_border = NULL,
    top_border    = "thin",
    left_border   = NULL,
    right_border  = NULL
  )
  
  border_row_l <- border_row - 1
  
  # ---- right border on the last column (from top of table to bottom border row) ----
  left_dims <- paste0(last_col_let, first_table_row, ":", last_col_let, border_row_l)
  wb$add_border(
    sheet = sheet,
    dims  = left_dims,
    right_border  = NULL,
    top_border    = NULL,
    left_border   = "thin",
    bottom_border = NULL
  )

# If there is XXX cell, add it to the footnotes
xxx_cell <- out_glob$a_cells |> select(row, col, character) |> filter(str_detect(character, "XXX"))

if (nrow(xxx_cell) > 0) {
  txt_base <- xxx_cell |> select(character)

txt_base <- txt_base |> separate(col = character, into = c("a1", "a2", "a3"), sep = " ", extra = "merge") |> 
  mutate(a1 = paste0(a1, " ")) |> 
  mutate(a3 = paste0(" ", a3))

txt <- paste0(
  openxlsx2::fmt_txt(
    x = txt_base$a1,
    vert_align = "superscript",
    font = "Arial",
    size = 8
  ),
  openxlsx2::fmt_txt(
    x = as.character(xxx),
    vert_align = "baseline",
    font = "Arial",
    size = 8
  ),
  openxlsx2::fmt_txt(
    x = txt_base$a3,
    vert_align = "baseline",
    font = "Arial",
    size = 8
  )
)

wb$add_data(sheet = sheet, x = data.frame(note = I(list(txt))), 
           startRow = xxx_cell$row, startCol = xxx_cell$col, colNames = FALSE)
}




  openxlsx2::wb_save(wb, dest)
  invisible(NULL)
}














