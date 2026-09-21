# Table 6.2: run one section at a time in RStudio or Positron.
# Use your actual survey files when the file selection dialogs open.
# No app or browser is launched. Preparation runs your selected prep script.

library(micsPlusTableR)
library(dplyr)

# 1. Select the input files -------------------------------------------------
message("Select the raw household .sav file")
hh_path <- file.choose()
message("Select the raw household-member .sav file")
hl_path <- file.choose()
message("Select the survey preparation .R script")
prep_script <- file.choose()

plan_path <- "TKM MICSPlus 2025-26 Wave2_TabPlan_20260915.xlsx"
if (!file.exists(plan_path)) {
  message("Select the Excel tabulation plan")
  plan_path <- file.choose()
}
sheet <- "6.2"

# 2. Run preparation and inspect the resulting hh ---------------------------
s <- mics_session()
prepare_mics_data(
  session = s,
  hh_path = hh_path,
  hl_path = hl_path,
  prep_script = prep_script,
  output_dir = "micsPlusTableR-output"
)

# The preparation script creates both hh and hl inside s.
# These are convenient workspace copies for inspection.
hh <- s$hh
hl <- s$hl
print(dim(hh))
print(names(hh))
print(head(hh))
# View(hh)
# If you edit the workspace copy, put it back before calculating: s$hh <- hh

# 3. Read worksheet 6.2 -----------------------------------------------------
plan <- read_mics_tabulation(
  session = s,
  path_tab_excel = plan_path,
  sheet = sheet
)
print(mics_table_name(s))
print(plan$tab_direction)  # "h" = horizontal, "v" = vertical

# 4. Inspect everything the calculator will use -----------------------------
# Source, filters/unfilter(), mutate instructions and weights from Excel.
print(plan$filter_row, width = Inf)
# Original row/column instructions and the statistics in each cell.
print(plan$tab_r, width = Inf)
print(plan$tab_c, width = Inf)
print(select(plan$tab, any_of(c("row_index", "col_index", "stat_type",
                               "display_digits"))), n = Inf)

# These functions parse instructions; they do not calculate estimates.
rows <- row_condition_f(plan$tab_r)
columns <- col_condition_f(plan$tab_c)
print(rows, width = Inf)
print(columns, width = Inf)

# 5. Calculate with the appropriate function --------------------------------
# A prep script can map a sheet to a supplied extra table. In that case,
# tabulate_mics_table() handles the mapping instead of calculating from hh/hl.
is_extra <- sheet %in% s$tables_extra
if (is_extra) {
  results <- tabulate_mics_table(s)
} else if (plan$tab_direction == "h") {
  results <- tabulate_h(s)
} else {
  results <- tabulate_v(s)
}

# Alternatively, tabulate_mics_table(s) selects the route automatically and
# adds variable-explanation labels. Run it INSTEAD of the branch above if wanted.
# results <- tabulate_mics_table(s)

# Numeric estimates and, where available, their calculation metadata.
print(results |>
  select(any_of(c("row_index", "col_index", "row_header", "col_header",
                  "stat_type", "df", "filt1", "filt2", "calc_chain",
                  "weight_var", "row_logic", "col_logic", "value",
                  "n_unw", "value_f_view"))), n = Inf, width = Inf)

# 6. Reproduce one horizontal cell, inspecting each data stage ----------------
# calc_cells() is the horizontal cell calculator. This section is skipped for
# vertical plans and supplied extra tables, which use different calculations.
if (!is_extra && plan$tab_direction == "h") {
  # Start with the first calculated cell. To choose a different Excel cell,
  # replace this with e.g. filter(results, row_index == 12, col_index == 7).
  cell <- results |> filter(!is.na(stat_type)) |> slice(1)
  stopifnot(nrow(cell) == 1L)
  print(cell, width = Inf)

  # Run one stored filter/calculation instruction using the session's data
  # and functions, as the engine does. Instructions come from your workbook.
  apply_instruction <- function(data, instruction) {
    if (is.na(instruction) || !nzchar(trimws(instruction))) return(data)
    eval(
      rlang::parse_expr(paste0("data |> ", instruction)),
      envir = list(data = data),
      enclos = s
    )
  }

  data_source <- s[[cell$df[[1]]]]
  data_global <- apply_instruction(data_source, cell$filt1[[1]])
  data_secondary <- apply_instruction(data_global, cell$filt2[[1]])
  data_calculated <- apply_instruction(data_secondary, cell$calc_chain[[1]])

  # filt2 is NA after unfilter(): data_secondary then equals data_global.
  # Means retain the secondary filter until a new filter or unfilter().
  print(data.frame(
    stage = c("prepared source", "global filter", "secondary filter",
              "block calculations"),
    records = c(nrow(data_source), nrow(data_global), nrow(data_secondary),
                nrow(data_calculated))
  ))
  # View(data_global)
  # View(data_secondary)
  # View(data_calculated)

  row_spec <- plan$tab_r |> filter(row_index == cell$row_index[[1]])
  col_spec <- columns |>
    filter(col_index == cell$col_index[[1]]) |>
    mutate(
      col_logic = gsub("[\r\n]+", " ", col_condition),
      col_condition = if_else(trimws(col_logic) == "ph", "TRUE", col_logic),
      col_var_name = if_else(trimws(col_logic) == "ph",
                             ".mics_placeholder_col", col_var_name)
    )
  stat_spec <- plan$tab |>
    filter(row_index == cell$row_index[[1]], col_index == cell$col_index[[1]]) |>
    select(any_of(c("row_index", "col_index", "stat_type", "display_digits")))

  print(row_spec, width = Inf)
  print(col_spec, width = Inf)
  print(stat_spec, width = Inf)
  print(cell$weight_var)

  # Leave row and column predicates to calc_cells(): for a horizontal
  # percentage, the row defines the base and the column defines the outcome.
  # Pre-filtering both here would incorrectly change that denominator.
  single_cell <- calc_cells(
    df = data_calculated,
    tab_r = row_spec,
    tab_c3 = col_spec,
    tab = stat_spec,
    weight_var = cell$weight_var[[1]],
    weighted = TRUE
  )
  print(single_cell, width = Inf)
  print(data.frame(table_value = cell$value, reproduced_value = single_cell$value))
  stopifnot(isTRUE(all.equal(cell$value, single_cell$value)))
  # This verifies the raw estimate. Suppression and display formatting are
  # added by tabulate_h(), and can be inspected in results above.
}

# 7. View the table and run the consistency checks ---------------------------
# The direct direction functions support index/logic previews. A header
# preview additionally needs labels attached by tabulate_mics_table().
preview_numeric <- pivot_mics_table(s, results, type = "index")
preview_display <- pivot_mics_table(s, results, formatted = "view", type = "index")
print(preview_numeric)
print(preview_display)
checks <- check_mics_table(s, results)
print(checks$summary)
# str(checks$details, max.level = 1)

# A single check can also be run separately:
row_check <- row_group_total_check(s, results)
print(row_check$status)
print(row_check$issues)

# OPTIONAL: step through the actual calculator's R code ---------------------
# For a horizontal plan, run these two lines individually in the console:
# debugonce(s$calc_cells)
# results <- tabulate_h(s)
# At Browse[...]> use n = next expression, c = continue, Q = quit debugging.
# Inspect df, tab_r, tab_c3, tab, and weight_var at the first pause.
# For the surrounding filter/block logic use debugonce(s$tabulate_h).
# For a vertical plan use debugonce(s$tabulate_v), then tabulate_v(s).
