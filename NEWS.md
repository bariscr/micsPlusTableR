# micsPlusTableR 0.3.2

* Replace Data Exploration's flat frequency list with SPSS-style reports per
  variable: selectable descriptive statistics, valid and missing counts,
  frequency percentages, valid percentages, cumulative percentages, and totals.
  Use labelled values, shaded stubs, aligned numbers, and responsive layouts.
  Honor the Unweighted selection even if a weight variable was entered earlier.

* Display missing (`NA`) medians as `-`, alongside the existing `NaN` rule,
  in table previews and Excel exports. Include the dash footnote once whenever
  these markers occur, including tables without small-sample suppression.

* Apply an 11.25-point minimum row height to existing and generated conditional
  footnotes in Excel exports. Force conditional footnote rows to exactly 11.25
  points and clear their cell indentation. For existing authored footnotes,
  preserve rows already at or above the minimum, including taller rows
  inherited from the worksheet default.

* Set the default Excel export zoom to 100% instead of 140%.

* Avoid rebuilding the wave selector when country selections share the same
  available waves. This prevents a delayed update from restoring Wave 1 after
  Wave 2 is selected, including the Turkmenistan file-upload workflow.

# micsPlusTableR 0.3.1

* Use one Excel-writing workflow for single-table and multi-sheet exports,
  including checks, suppression, and footnotes. Preserve the selected table's
  plan when another sheet is processed, and remove obsolete generated
  suppression notes when rewriting a workbook. Empty-group means still display
  as `-` even in tables where small-sample suppression is disabled.

* Make supplied extra tables honor the plan's suppression flag, which requires
  `n_unw` or `n_unw2` immediately before `IDX`. Write conditional suppression
  footnotes only for eligible tables with the corresponding display markers;
  negative estimates no longer trigger the zero-denominator footnote.

* Fix row-condition mutations in horizontal, vertical, and standalone cell calculations: support nested calls and separators, execute top to bottom, preserve earlier results, and retain repeated setup steps. Vertical shared column-B setup now executes before row mutations.

- Restore condition-row mutations in column order within horizontal filter
  blocks, so later recodes cannot overwrite earlier column calculations.
  Apply shared calculations once per block. Parse multiline mutations and
  complete predicates, with `- - -` separating setup from the condition.

- Clarify that standalone sessions are ordinary R environments and do not
  launch the app. Add examples for inspecting a plan without survey data
  and calculating with data already prepared in the workspace.

- Ask for browser confirmation before closing, reloading, or leaving the app
  tab after user interaction, to help prevent accidental session loss.

- Keep secondary horizontal filters active across all statistic changes,
  including means. Add `unfilter()` in the condition row to clear the secondary
  filter from that column onward while retaining column B's global filter.
  A new `filter(...)` still replaces the preceding secondary filter.

- Expand the User's Guide with output-oriented row and column condition
  instructions, repeated total aliases, household-member bases, and worked
  layouts for percentages, means, and vertical distributions. Add a matching
  worksheet-conditions reference topic explaining `total1 == 1`,
  `totalHL == 1`, `hhmembers`, and their calculation limits.

- Consolidate application instructions in the User's Guide and direct R
  examples in the package reference. Remove the duplicated getting-started
  vignette and its knitr/rmarkdown build dependencies.

- Add optional `d=` display precision to every worksheet statistic, for example
  `mean(HCS8, d=0)`, `mean(d=1)`, `p(d=2)`, `n(d=1)`, and `p(100, d=2)`.
  Keep existing defaults when omitted. Apply overrides to Tabulator displays,
  both Excel workbooks, and long-format DECIMALS/display values while preserving
  full numeric estimates and suppression markers. Keep display metadata separate
  from statistic identity so direction, count bases, checks, and filters are unchanged.

- Format mean expressions such as `mean(HCS8)` with thousands separators and
  exactly one decimal place in the formatted preview, matching Excel's number
  format. Apply the same display rule to vertical and supplied extra tables,
  preserving numeric estimates, filters, and suppression thresholds/markers.

- Silence routine Excel name-repair and implicit-join messages and remove
  batch table-name debug prints. Mute consistency-check console output inside
  Shiny while retaining its results in the UI and diagnostics for direct R calls.

- Retain the workbook returned by the alignment update so table values are
  right-aligned in both Excel output formats, matching the preview.

- Always bold the leading Total rows, including their labels and values, in
  the table preview and both Excel output formats.

- Keep valid estimates visible when a new filter separates them from their
  right-hand `n_unw` count. Restore unmatched suppression counts from the next
  count on the same row and data source without replacing existing matches
  or changing suppression thresholds. Cover ordinary and supplied extra tables,
  including an expense mean in F followed by a new filter in G.

- Apply the condition-row filter in column B globally to every horizontal
  block, regardless of the position of later filters. Preserve secondary
  filters across all statistic changes so percentages, means, and their
  bases use the same population. A new filter replaces the secondary filter;
  `unfilter()` clears it while B remains active.
  Keep vertical filtering, statistic calculations, and row predicates intact.
  Prevent mutate-only entries from prematurely cutting off horizontal columns.
  Add regression tests and explain the rules in the HTML/PDF User's Guide
  and function reference.

- Display undefined estimates from empty groups with the standard MICS `-`
  marker in the app and Excel exports while retaining `NaN` in the underlying
  numeric results. The existing zero-denominator footnote is added
  automatically when this marker appears.

- Audit runtime dependencies; remove unused DiagrammeR, gt, sjlabelled, and
  stringdist dependencies and obsolete namespace imports. Keep knitr as a
  vignette-building Suggests rather than a runtime Import.
- Install all eight preparation-only packages with the package as required
  Imports, including memisc for Jamaica FIES helpers. Document their purposes
  separately without adding an installation step for users.
- Add check_prep_dependencies() and install_prep_dependencies(), automatic
  preflight checks in both preparation workflows, and separate dependency docs.

- Rename the internal engines to `tabulate_v()` and `tabulate_h()` and expose
  session-based entry points. Replace direct calls to the old names ending in 2.
- Export and document 16 smaller tabulation, parsing, cleaning, and individual
  consistency-check helpers for focused testing from R.
- Add worksheet, filter-block, expression, statistic, and cell context to
  calculation errors; validate missing data, weights, plan markers, extra-table
  shape, comparison dimensions, and public arguments.
- Add regression coverage for both directions, filter-block layouts, weighted
  calculations, individual checks, and actionable errors. Update the guide and
  scripting vignette with troubleshooting and individual-step examples.

- Add a Download Files tab before Data Preparation. Download one, several,
  or all published survey folders, with a destination defaulting to the user's
  project captured at app launch. Support folder browsing, typed paths, a reset
  to the project, overwrite control, and selection/status feedback.
- Document the app download workflow and link to the permission-based data
  access page at https://mics.unicef.org/surveys.

- Add `list_survey_files()` and `download_survey_files()` for the separate
  public `bariscr/micsPlusTableR-Files` repository. Download plans, preparation scripts, and reference assets for
  all surveys, selected survey folders, or exact files while preserving subfolders.
  Survey folders are named `JAM_W1`, `MNG_W2`, etc. Household/member microdata
  are excluded and must be obtained separately from MICS Plus with permission.
- Protect existing files by default, stage network downloads before writing
  destinations, and resolve a single Git commit for each request.
- Preserve the maintained guide source and rendered assets during devtools builds.
- Add direct ZIP links and beginner download instructions to the user guide,
  README, and workflow vignette. Add `jsonlite` as an explicit dependency.

# micsPlusTableR 0.3.0

- Mark the changed workflow and separation of `micsPlusTableR` from the previous
  project as a standalone package with a minor version increase.
- Document the versioning policy, release checklist, and planned 1.0.0 milestone
  for the start of actual survey use in the maintainer-only
  `micsPlusTableR-manager/docs/setup-and-maintenance.md`.
- Expand the user guide with R/RStudio installation, new and existing survey
  projects, package setup, and the complete path from file selection to Excel.
  Document alternative R editors and keep user workflows in the package docs.
