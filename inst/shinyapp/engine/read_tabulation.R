# R/read_tabulation.R

#' Read an Excel tabulation plan and build workflow objects
#'
#' Reads a tabulation sheet from an Excel workbook and constructs multiple
#' intermediate workflow objects (e.g., `tab_org`, `tab_base`, `tab`, headers,
#' indentation/group info, IDX-related flags), assigning them to the calling
#' environment using `<<-`.
#'
#' @details
#' **Side effects:** This function assigns the following objects globally using `<<-`:
#' `tab_org`, `empty_rows`, `empty_cols`, `condition_row_index`, `tab_base`,
#' `filter_row`, `tab_r`, `tab_c`, `col_header`, `row_header`, `a_cells`,
#' `indent_rows`, `group_info`, `idx_col`, `is_n_unw`, `is_supp`,
#' `is_total_col`, `tab_direction`, `condition_write`, and finally `tab`.
#'
#' This design supports a workflow where subsequent functions consume these
#' objects without needing a large return value.
#'
#' @param path_tab_excel Path to the Excel file containing the tabulation plan.
#' @param sheet Sheet name or index to read.
#'
#' @return Invisibly returns `NULL`. (Primary output is via globally assigned objects.)
#'
#' @examples
#' \dontrun{
#' read_tabulation("excel-tables/tabulation.xlsx", sheet = "Table1")
#' # After running, objects like tab_org, tab_base, tab, etc. exist globally.
#' }
#'
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace str_detect fixed str_extract str_trim
#' @importFrom dplyr mutate relocate row_number filter pull select where fill rename
#' @importFrom dplyr between left_join case_when if_else distinct summarise n
#' @importFrom tidyr pivot_longer
#' @importFrom tidyxl xlsx_cells xlsx_formats
read_tabulation <- function(path_tab_excel, sheet) {
  
  mics_file(path_tab_excel, "path_tab_excel")
  mics_sheet(path_tab_excel, sheet)
  # tab_org --------------------------------------------------
  # We get the original table, nothing is changed except for column 3 is filled
  tab_org <- 
    read_excel(path_tab_excel,
               sheet = sheet,
               col_names = FALSE,
               .name_repair = "unique_quiet")
  
  if (ncol(tab_org) < 3L || nrow(tab_org) < 4L) {
    stop("Worksheet '", sheet, "' is too small for a tabulation plan. It needs at least three columns and a data-source condition in B4:B7.", call. = FALSE)
  }
  # The column names are now column indices
  names(tab_org) <- str_replace(names(tab_org), "...", "")
  
  tab_org <-
    tab_org |> 
    mutate(row_index = row_number()) |> 
    relocate(row_index, .before = 1) 
  #fill(`3`) # to detect stat_type in the first part - this is necessary if there is another rowwise block - Sonrasında dolduruyorum, gereksiz gibi, denemeden sonra sil
  
  out_glob <- list(tab_org = tab_org)
  # ---------------------------------------------------------
  
  
  # empty_rows --------------------------------------------------
  # To be used in the comparison table ----
  # to compare with spss table
  ## Get empty rows
  
  empty_rows <-
    tab_org |> 
    filter(is.na(`2`)) |> 
    pull(row_index)

  out_glob$empty_rows <- empty_rows
  # -------------------------------------------------------------
  empty_cols <- get_blank_cols(tab_org)
  
  out_glob$empty_cols <- empty_cols
  # condition_row_index -----------------------------------------
  # Get condition row
  if (isTRUE(stringr::str_detect(tab_org[4, 3], fixed(".sav")))) {
    condition_row_index <- 4
  } else if (isTRUE(stringr::str_detect(tab_org[5, 3], fixed(".sav")))) {
    condition_row_index <- 5
  } else if (isTRUE(stringr::str_detect(tab_org[6, 3], fixed(".sav")))) {
    condition_row_index <- 6
  } else if (isTRUE(stringr::str_detect(tab_org[7, 3], fixed(".sav")))) {
    condition_row_index <- 7
  } else {
    stop("Worksheet '", sheet, "' has no .sav data-source condition in cells B4:B7. Add the household or household-member source and filter to the plan.", call. = FALSE)
  }
  
  out_glob$condition_row_index <- condition_row_index
  
  # ------------------------------------------------------------
  
  
  # tab_base --------------------------------------------------
  # tab base is cell based having row_index, col_index and stat_type to be applied
  tab_base <-
    tab_org |> 
    filter(row_index > condition_row_index - 1) |> 
    select(1, 4:(ncol(tab_org))) |> 
    select(where(~ !all(is.na(.)))) |> 
    filter(row_index > condition_row_index) |> 
    fill(1:(last_col())) |> 
    pivot_longer(-row_index,
                 names_to = "col_index",
                 values_to = "stat_type") |> 
    fill(stat_type) |> 
    mutate(col_index = as.numeric(col_index)) |> 
    filter(!is.na(stat_type)) # Drop cells where there is no stat_type
  
  out_glob$tab_base <- tab_base
  # -------------------------------------------------------------
  
  
  
  ## Filter row --------------------------------------------------
  # Get all filters and data ----
  
  filter_row <- tab_org |> 
    filter(row_number() == condition_row_index) |> 
    pivot_longer(`1`:last_col(),
                 names_to = "col_index",
                 values_to = "conditions"
    ) |> 
    # Filter row is created depending on the availability of .sav, mutate or filter
    filter(str_detect(conditions, ".sav") | 
             str_detect(conditions, "filter") |
             str_detect(conditions, "mutate")   
    ) |> 
    mutate(col_index = as.numeric(str_replace(col_index, "...", ""))) |> 
    mutate(df = ifelse(
      grepl("\\.sav", conditions),
      sub("\\.sav.*", "", conditions),
      NA
    )) |> 
    mutate(filter_condition = ifelse(
      str_detect(conditions, "- - -"),
      str_extract(conditions, "filter\\([\\s\\S]*?\\)(?=\\s*- - -)"),
      str_extract(conditions, "filter\\([\\s\\S]*\\)")
    )) |> 
    mutate(
      calculation = if_else(
        str_detect(conditions, "(?m)^\\s*mutate\\("),
        str_trim(str_extract(conditions, "(?m)^\\s*mutate\\([^\\n\\r]*")),
        NA_character_
      )
    ) |> 
    mutate(weight = ifelse(
      grepl("weight by", conditions),
      sub(".*weight by\\s*", "", conditions),
      NA
    )) |> 
    fill(df, weight)
  
  out_glob$filter_row <- filter_row
  ## ---------------------------------------------------------------
  
  
  # tab_r ---------------------------------------------------------
  # Rowwise conditions to be applied to each row
  # Get conditions in column B
  tab_r <- 
    tab_org[3] |> 
    rename(row_lgc = 1) |> 
    mutate(row_index = row_number()) |> 
    filter(row_index > condition_row_index) |> 
    filter(!is.na(row_lgc)) |>
    mutate(row_lgc = normalize_condition_text(row_lgc))

  out_glob$tab_r <- tab_r
  ## ---------------------------------------------------------------
  
  # tab_c ---------------------------------------------------------
  # Get column conditions in the selected row
  tab_c0 <- 
    tab_org[condition_row_index,] |> 
    pivot_longer(-1) |> 
    rename(col_lgc = value) |> 
    # assign a col_index that would be follow the tabulation plan and equal to the one from tab_org and tab_base
    mutate(col_index = row_number()) |> 
    select(col_lgc, col_index) |> 
    filter(col_index >= 3) |> 
    filter(!col_index %in% empty_cols) |> 
    fill(col_lgc) |> 
    mutate(col_lgc = trimws(col_lgc)) |> 
    mutate(
      col_lgc = if_else(
        str_detect(col_lgc, "(?s)(?:---|-\\s*-\\s*-)"),
        str_trim(str_replace(col_lgc, "(?s).*?(?:---|-\\s*-\\s*-)\\s*", "")),
        col_lgc
      )
    ) |> 
    mutate(col_lgc = (if_else(str_detect(col_lgc, ".sav"), 
                              NA, 
                              col_lgc
    )))
  
  # Enabling tatal1 == 1 alike logic in cell
  tab_c0 <-
    tab_c0 |> 
    mutate(col_lgc = case_when(
      col_lgc %in% c("total1 == 1",
                     "total2 == 1",
                     "total3 == 1",
                     "total4 == 1",
                     "total5 == 1"
      ) ~ "total == 1",
      col_lgc %in% c("totalHL == 1",
                     "totalHL1 == 1",
                     "totalHL2 == 1",
                     "totalHL3 == 1",
                     "totalHL4 == 1"
                     ) ~ "hhmembers",
      TRUE ~ col_lgc
    ))
  
    tab_c <-
    tab_c0 |> 
    mutate(col_lgc = case_when(
      col_lgc %in% c("all") ~ "total == 1",
      TRUE ~ col_lgc
    ))
  
  out_glob$tab_c <- tab_c
  
  ## ---------------------------------------------------------------
  
  
  ## tab------------------------------------------------------------
  # join all tab info
  tab <-
    tab_base |> 
    left_join(tab_r, by = c("row_index")) |> 
    left_join(tab_c, by = c("col_index"))
  
  ## ---------------------------------------------------------------
  
  ## col_header------------------------------------------------------------
  col_header <-
    tab_org |> filter(between(row_index, condition_row_index - 2, condition_row_index - 1)) |> 
    fill(1:last_col()) |> 
    filter(row_number() == 2) |> 
    select(-row_index) |> 
    pivot_longer(1:last_col(),
                 names_to = "col_index",
                 values_to = "col_header") |> 
    mutate(col_index = as.numeric(col_index))

  out_glob$col_header <- col_header
  ## ---------------------------------------------------------------
  
  ## row_header------------------------------------------------------------
  row_header <-
    tab_org |> select(1:2) |> 
    rename(row_header = 2)

  out_glob$row_header <- row_header
  ## ---------------------------------------------------------------
  
  ## Add the indentation information ------------------------------------
  a_cells <- xlsx_cells(path_tab_excel, sheets = sheet)
  
  out_glob$a_cells <- a_cells
  
  formats <- xlsx_formats(path_tab_excel)
  
  # We only need the row and indentation info
  cells_indented <-
    a_cells %>%
    filter(col == 1) |> 
    mutate(indent = formats$local$alignment$indent[local_format_id]) %>%
    filter(!is.na(indent) & indent > 0) %>%
    select(row, indent)
  
  
  # We also create indent_rows as a unique row vector for convenience
  indent_rows <- cells_indented |> distinct(row, indent)
  
  out_glob$indent_rows <- indent_rows
  
  is_indent <- max(indent_rows$indent) > 1
  # We add this info to tab ------------------------------------------
  tab <-
    tab |> 
    left_join(cells_indented, by = c("row_index" = "row")) 
  
  # Add group to tab ----
  group_info <-
    a_cells |> 
    filter(col == 2) |> 
    mutate(character = if_else(character == "", NA_character_, character)) |> 
  mutate(
    grp = cumsum(!is.na(character) & lag(is.na(character), default = TRUE)),
    grp = if_else(is.na(character), NA_integer_, grp)   # keep separators as NA 
  ) |> select(row, grp) 
  
  out_glob$group_info <- group_info

  tab <-
    tab |> 
    left_join(group_info, by = c("row_index" = "row"))

  out_glob$tab <- tab
  
  # Where is IDX - find its column
  idx_col <- a_cells |> filter(character == "IDX") |> pull(col)
  out_glob$idx_col <- idx_col

  is_n_unw <- a_cells |> filter(col == idx_col - 1, character == "n_unw") |> nrow()
  out_glob$is_n_unw <- is_n_unw

  is_100 <- sum(c("100", "100.0") %in% tab$stat_type)

# Whether this has an n_unw before IDX, if so it will be suppressed
  is_supp <- any(
  tab_base$col_index == (idx_col - 1) &
    tab_base$stat_type %in% c("n_unw", "n_unw2"),
  na.rm = TRUE
)
  
  out_glob$is_supp <- is_supp

  # Whether there is a column total
  is_total_col <- any(tab_c0$col_lgc %in% c("total == 1", "ph"))
  out_glob$is_total_col <- is_total_col

  # Tabulation type
  tab_direction <- if_else(is_n_unw > 0 | is_100 > 0, "h", "v")
  
  out_glob$tab_direction <- tab_direction

  # Get col conditions and row condition to write
#col_condition_write <-  
#  col_condition_f(tab_c) |> 
#  select(col_index, value = col_condition0) |> 
#  mutate(row_index = condition_row_index)

col_condition_write <-
  a_cells |> filter(col %in% tab_c$col_index, row == condition_row_index) |>
  select(col_index = col, character, numeric) |>
  mutate(value = if_else(!is.na(character), character, as.character(numeric))) |>
  select(col_index, value) |>
  mutate(row_index = condition_row_index)

#row_condition_write <-
#  row_condition_f(tab_r) |> 
#  select(row_index, value = row_condition0) |> 
#  mutate(col_index = 2) 

row_condition_write <-
  a_cells |> filter(col == 2, row %in% tab_r$row_index) |>
  select(row_index = row, value = character) |> 
  mutate(col_index = 2)

condition_write <-
  col_condition_write |> 
  bind_rows(row_condition_write)
  
  out_glob$condition_write <- condition_write

# Build headers ------------------------------------------------------------
build_headers <- function(tab_org,
                          condition_row_index,
                          start_row = 3,
                          drop_cols = c("1", "2")) {

  end_row <- condition_row_index - 1

  # ---- 1) long header cells (fill within each column) ----
  header_long <-
    tab_org %>%
    filter(between(row_index, start_row, end_row)) %>%
    select(-any_of(drop_cols)) %>%
    fill(everything()) %>%
    select(where(~ !all(is.na(.)))) %>%
    pivot_longer(-row_index, names_to = "col_index", values_to = "value") %>%
    #group_by(col_index) %>%
    fill(value, .direction = "down") 

  # ---- 2) wide: one row per col_index, one column per header row_index ----
  header_wide <-
    header_long %>%
    pivot_wider(names_from = row_index, values_from = value) %>%
    mutate(col_index = as.numeric(col_index)) %>%
    arrange(col_index)

  level_cols <- setdiff(names(header_wide), "col_index")
  if (length(level_cols) == 0) {
    return(list(
      header_leaf = tibble(col_index = numeric(0), label = character(0)),
      header_groups = list()
    ))
  }

  # keep numeric order of header rows
  level_nums <- suppressWarnings(as.integer(level_cols))
  ord <- order(level_nums)
  level_cols <- level_cols[ord]
  level_nums <- level_nums[ord]

  # ---- 3) your "dedupe if equals next level" logic, generalized ----
  for (i in seq_len(length(level_cols) - 1)) {
    a <- level_cols[i]
    b <- level_cols[i + 1]

    header_wide[[a]] <- dplyr::if_else(
      !is.na(header_wide[[a]]) & !is.na(header_wide[[b]]) & header_wide[[a]] == header_wide[[b]],
      NA_character_,
      as.character(header_wide[[a]])
    )
  }

  # ---- 4) drop levels that don't exist (all NA after dedupe) ----
  level_cols <- level_cols[!vapply(level_cols, function(nm) all(is.na(header_wide[[nm]])), logical(1))]

  # ---- 5) construct outputs ----
# ---- 5) construct outputs ----
leaf_level <- tail(level_cols, 1)

header_leaf <-
  header_wide %>%
  transmute(col_index, label = .data[[leaf_level]])

upper_levels <- head(level_cols, -1)

if (length(upper_levels) == 0) {
  header_groups <- list()
} else {
  header_groups <- map(rev(upper_levels), function(lvl) {
    header_wide %>%
      filter(!is.na(.data[[lvl]])) %>%
      group_by(label = .data[[lvl]]) %>%
      summarise(
        col_start = min(col_index),
        col_end   = max(col_index),
        .groups   = "drop"
      )
  })
  names(header_groups) <- paste0(
    "level_",
    rev(level_nums[match(upper_levels, names(header_wide))])
  )
}

  list(
    header_leaf = header_leaf,       # per-column label (bottom-most level)
    header_groups = header_groups,   # list of merge ranges for higher levels
    header_wide = header_wide        # optional: keep for debugging/inspection
  )
}

# ---- usage ----
hdr <- build_headers(
  tab_org = tab_org,
  condition_row_index = condition_row_index,
  start_row = 3,
  drop_cols = c("1", "2")
)

pluck_or_null <- function(x, i) {
  if (is.null(x) || length(x) < i) return(NULL)
  x[[i]]
}

out_glob$header_1 <- hdr$header_leaf
out_glob$header_2 <- pluck_or_null(hdr$header_groups, 1)
out_glob$header_3 <- pluck_or_null(hdr$header_groups, 2)
out_glob$header_4 <- pluck_or_null(hdr$header_groups, 3)
out_glob$header_level_n <- ncol(hdr$header_wide) - 1

# Variables explanations

# Using bold rows
bold_rows <-
  a_cells %>%
  filter(col == 1) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>%
  filter(bold %in% TRUE) %>%
  pull(row) 
  
# Variables explanations
variable_exp <-
  row_header |> 
    filter(row_index %in% c(bold_rows) & 
           row_index > condition_row_index & 
           !is.na(row_header))   
  
data_rows <- tab_base |> distinct(row_index)  
  
out_glob$variable_exp <-
  data_rows |> 
  left_join(variable_exp, by = "row_index") |>
  fill(row_header) |> 
  filter(!row_index %in% empty_rows) |> 
  rename(variable_exp = row_header)

# IDX tab
idx_tab <-
  xlsx_cells(path_tab_excel, sheets = "IDX") |> 
  select(row, col, character) |> 
  filter(!is.na(character)) |> 
  filter(str_starts(character, "Table")) |> 
  separate(character, into = c("table", "table_code", "table_name"), sep = " ", extra = "merge") |> 
  select(-table)

out_glob$idx_tab <- idx_tab

# I assign the out_glob to the global environment
  out_glob <<- out_glob

}

