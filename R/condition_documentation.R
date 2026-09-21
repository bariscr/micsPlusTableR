#' Worksheet row and column conditions
#'
#' Prepare conditions around the required output's unit, eligible population,
#' comparison groups, and statistic. The User's Guide section Define row and
#' column conditions for your output provides worked worksheet layouts.
#'
#' @section Location and population:
#' Row predicates are written in column B below the condition row. Column
#' predicates are written from C onward on the condition row identified by
#' the source instruction in B4:B7. Labels alone do not select records.
#' Ordinary predicates use prepared variables, for example `area == 1`,
#' `age >= 15 & age <= 49`, or `response %in% c(1, 2)`.
#'
#' After dataset selection and filters, horizontal percentages use the row
#' predicate as the base and the column predicate as the outcome. Vertical
#' percentages use the column predicate as the base and the row predicate as
#' the outcome. Counts and numeric summaries normally use both predicates.
#' A count in a neighboring cell does not define a percentage's denominator.
#' Missing-outcome handling is documented in [statistic-types].
#'
#' The worksheet reader omits blank row-condition cells. The standalone
#' [row_condition_f()] helper instead treats blank predicates as TRUE.
#' Column instructions can fill from the preceding column. Write changed
#' predicates explicitly. In a multiline column instruction, use a separator
#' `- - -` after source/filter/weight/calculation instructions. The complete
#' predicate below the separator can span multiple lines.
#'
#' @section Creating variables in the condition row:
#' Use `mutate(...)` above `- - -` to create variables for column conditions.
#' For example:
#' ```
#' mutate(WS1R = if_else(WS1 %in% c(11, 12), 1, 0))
#' - - -
#' WS1R == 1 &
#' between(WS4, 1, 30)
#' ```
#' Within a horizontal filter block, a mutation applies at its own column and
#' continues right. A later mutation can use an earlier variable or redefine
#' it for subsequent columns without changing earlier results. Column B's
#' calculation is shared by the filter blocks and runs once per block. A
#' mutation does not change which secondary filter is active. Filters still
#' run before these calculations. Use the preparation script for variables
#' needed by filters or across different filter blocks. Worksheet mutations
#' operate on temporary data; they do not add columns to prepared `hh` or `hl`.
#' Multiline and multiple `mutate(...)` instructions are supported.
#'
#' @section Total aliases in worksheet columns:
#' Maintained preparation scripts set `total = 1` in household and member data.
#' `total == 1` therefore selects the records remaining under the applicable
#' filters and the other axis's predicate.
#' The worksheet reader converts these exact column forms:
#' \itemize{
#'   \item `total1 == 1` through `total5 == 1` become `total == 1`.
#'   \item `totalHL == 1` and `totalHL1 == 1` through `totalHL4 == 1`
#'     become `hhmembers`.
#'   \item The legacy column form `all` becomes `total == 1`.
#' }
#' Numbered aliases accommodate repeated total/base columns in separate blocks.
#' Their suffix does not select a subgroup, change weights, or set decimals;
#' filters and source instructions define the block population. These names
#' need not exist as prepared variables when used as recognized column aliases.
#' Use the exact spelling and internal spacing shown above. Bare `total1`,
#' additional suffixes, and compound expressions such as
#' `total1 == 1 & area == 1` are not alias forms.
#'
#' Conversion belongs to [read_mics_tabulation()], not the predicate parsers
#' or calculators. Aliases in row predicates or inside filters are not
#' converted. Direct [calc_cells()] calls must use `total == 1` or the
#' supported `hhmembers` condition explicitly.
#'
#' @section Household-member bases:
#' For a horizontal `n_unw` or `n_unw1` cell with column condition `hhmembers`,
#' the calculator applies the row predicate and sums
#' `if_else(total == 1, 1, 0, missing = 0) * HLnum`, with missing
#' contributions removed. This counts people represented by household records.
#' It requires prepared `total` and numeric, complete household-size `HLnum`.
#' Two eligible household records with sizes 2 and 4 contribute 6 members;
#' ordinary `total == 1` with `n_unw` contributes 2 household records.
#'
#' Weighted member counts use ordinary `total == 1` with `n` and a suitable
#' member-based weight, such as the approved household weight multiplied by
#' `HLnum`. Member-based percentages use that weight with the desired outcome.
#' `totalHL` and `hhmembers` do not create or select a weight. For sizes 2 and 4
#' and household weights 1 and 3, the weighted member count is 14, compared
#' with 4 weighted households and 6 unweighted members.
#'
#' The member-size special case does not apply to vertical calculations or
#' other statistics. On one-record-per-member data, count qualifying records
#' with ordinary predicates and `n_unw`; summing household sizes on every
#' member record would overcount. Use member records for member-level subsets,
#' such as children selected by age, rather than the full household-size sum.
#'
#' @section Preparing repeated blocks:
#' For a horizontal percentage reporting an outcome, use its predicate (for
#' example `HCS7 == 1`) with `p`, followed by `total == 1` columns with
#' `n` and `n_unw` for the base. To average a numeric amount only among records
#' reporting it, place the eligibility/valid-value filter on the mean column,
#' then `- - -` and `total1 == 1`. Use `mean(HCS8)` there and `n`/`n_unw`
#' in following `total1 == 1` columns. The filter, not the numbered alias,
#' creates the restricted population. See [tabulate_h()] for filter scope.
#' Select survey-specific variables, valid-value rules, and weights before
#' adapting this pattern. A mean among positive reporters differs from a mean
#' across all eligible records with correctly prepared zeros for non-reporters.
#'
#' @seealso [read_mics_tabulation()], [statistic-types], [calc_cells()]
#' @name worksheet-conditions
NULL
