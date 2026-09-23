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
  
  # Parentheses and stars require suppression eligibility; missing-value
  # dashes also occur in tables that do not use suppression.
  vec <- if (isTRUE(out_glob$is_supp)) trimws(df$value_f) else character()
  
  is_1 <- any(stringr::str_detect(vec, "^\\([^*]+\\)$"), na.rm = TRUE)
  is_2 <- any(vec == "(*)", na.rm = TRUE)
  is_3 <- any(trimws(df$value_f) == "-", na.rm = TRUE) ||
    any(mics_missing_value_dash(df$value, df$stat_type), na.rm = TRUE)
  
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
  
  # The parsed plan fills statistic cells down through authored footnotes.
  # Locate the data boundary from calculated rows so those notes are not skipped.
  data_rows <- df$row_index[is.finite(df$row_index)]
  if (!length(data_rows)) data_rows <- tab$row_index
  last_table_row  <- max(data_rows, na.rm = TRUE) + 1
  first_table_row <- min(tab$row_index, na.rm = TRUE)

  # Rewriting an existing workbook must also remove notes that no longer apply.
  # Match only our standard notes below the data; preserve authored footnotes.
  existing <- tidyxl::xlsx_cells(dest, sheets = sheet)
  old_notes <- existing$address[existing$col == 1L &
    existing$row >= last_table_row & existing$character %in% c(foot1, foot2, foot3)]
  for (address in old_notes) {
    wb$add_data(sheet = sheet, x = "", dims = address, col_names = FALSE)
  }
  
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

  # Existing notes may start in any visible table column below the data.
  footnote_rows <- unique(existing$row[
    existing$row >= last_table_row & existing$col <= last_col_num2 &
      !is.na(existing$character) & nzchar(trimws(existing$character)) &
      !existing$address %in% old_notes
  ])
  
  # Close below authored notes even when no conditional notes are needed.
  # Borders use the top of the following row, and the right edge ends above it.
  border_row <- max(c(last_table_row, footnote_rows + 1L))
  
  # ---- write footnotes if any ----
  if (!is.null(txt) && length(txt) > 0) {
    start_row_foot_df <- max(c(last_table_row, footnote_rows + 1L))
    end_row_foot_df   <- start_row_foot_df + length(txt) - 1

    # Remove every internal horizontal rule before extending the note block.
    # Include blank rows and previously generated notes, whose closing border
    # may now fall inside the block. Preserve the top and vertical edges.
    old_note_rows <- existing$row[existing$address %in% old_notes]
    clear_start <- min(c(footnote_rows, old_note_rows, start_row_foot_df))
    clear_end <- max(c(old_note_rows, end_row_foot_df))
    wb$add_border(
      sheet = sheet,
      dims = paste0("A", clear_start, ":", last_col_let2, clear_end),
      top_border = NULL, bottom_border = "none", inner_hgrid = "none",
      left_border = NULL, right_border = NULL, update = TRUE
    )
    # A closing rule can also be stored as the next row's top border.
    wb$add_border(
      sheet = sheet,
      dims = paste0("A", clear_end + 1L, ":", last_col_let2, clear_end + 1L),
      top_border = "none", bottom_border = NULL,
      left_border = NULL, right_border = NULL, update = TRUE
    )
    
    start_row_foot <- start_row_foot_df + 1
    end_row_foot   <- start_row_foot + length(txt) - 1

    footnote_dims <- paste0("A", start_row_foot_df, ":A", end_row_foot_df)
    # Reset template indentation before writing conditional notes.
    wb$add_cell_style(
      sheet = sheet,
      dims = footnote_dims,
      indent = 0,
      wrap_text = FALSE
    )
    wb$set_row_heights(
      sheet = sheet,
      rows = seq.int(start_row_foot_df, end_row_foot_df),
      heights = 11.25
    )
    
    wb$add_data(
      sheet = sheet,
      x     = txt,
      dims  = paste0("A", start_row_foot_df)
    )
    
    # Style only the newly written conditional notes; preserve existing notes' wrapping.
    wb$add_font(
      sheet = sheet,
      dims  = footnote_dims,
      name  = "Arial",
      size  = 8
    )
    footnote_rows <- union(footnote_rows, seq.int(start_row_foot_df, end_row_foot_df))
    
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
footnote_rows <- union(footnote_rows, xxx_cell$row)
}

  # Apply the minimum even when no conditional notes are needed.
  # Preserve taller rows and the wrapping of existing authored notes.
  if (length(footnote_rows) > 0L) {
    row_attrs <- worksheet$sheet_data$row_attr
    heights <- as.numeric(row_attrs$ht[match(footnote_rows, row_attrs$r)])
    sheet_attrs <- openxlsx2::xml_attr(worksheet$sheetFormatPr, "sheetFormatPr")
    default_height <- as.numeric(sheet_attrs[[1]]["defaultRowHeight"])
    heights[is.na(heights)] <- default_height
    short_rows <- footnote_rows[is.na(heights) | heights < 11.25]
    if (length(short_rows) > 0L) {
      wb$set_row_heights(sheet = sheet, rows = short_rows, heights = 11.25)
    }
  }
  openxlsx2::wb_save(wb, dest)
  invisible(NULL)
}




