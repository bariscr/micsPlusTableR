# micsPlusTableR 0.3.1

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
  block, regardless of the position of later filters. Stop additional column
  filters at the first effective statistic change across each row; B remains
  active, and a stopped filter restarts only with an explicit new filter.
  Keep vertical filtering, statistic calculations, and row predicates intact.
  Prevent mutate-only entries from prematurely cutting off horizontal columns.
  Add regression tests and explain the rules in the HTML/PDF User's Guide,
  README, scripting vignette, and function reference.

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
