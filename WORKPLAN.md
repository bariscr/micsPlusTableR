# micsPlusTableR modernization workplan

## Intended outputs

1. An installable R package named `micsPlusTableR`.
2. A one-command launcher: `micsPlusTableR::run_app()`.
3. A session-isolated Shiny runtime that preserves the validated tabulation logic.
4. Explicit workbook creation and input validation helpers.
5. Automated package tests and repeatable package checks.
6. A concise, task-oriented PDF user's guide.
7. A supported explicit-session API for running the engine outside Shiny.

## Implementation phases

| Phase | Work | Acceptance criterion | Status |
|---|---|---|---|
| 1. Audit | Inventory scripts, inputs, outputs, dependencies, and global state | Current workflow and major risks are documented | Complete |
| 2. Package foundation | Add DESCRIPTION, NAMESPACE, launcher, build exclusions, and tests | Package loads and `run_app()` resolves the bundled app | Complete |
| 3. Runtime isolation | Bundle the existing engine and isolate mutable state per Shiny session | Engine functions no longer write workflow state into the user's global workspace | Complete |
| 4. Workflow cleanup | Replace child-script side effects with explicit functions and safer paths | Workbook creation returns explicit paths and avoids silent overwrites | Complete |
| 5. Verification | Run tests, package build/check, and application smoke tests | Checks pass or remaining external-code limitations are documented | Complete |
| 6. User documentation | Rewrite, render, and inspect the PDF guide | Installation, inputs, run sequence, outputs, troubleshooting, and limits are covered | Complete |
| 7. Scriptable API | Add supported explicit-session wrappers and real-data workflow tests | An analyst can prepare, tabulate, check, pivot, and write a table without Shiny | Complete |
| 8. Bootstrap integration | Absorb the former `child` scripts into package APIs and dependency metadata | The app and prep scripts run without a project-level `child` folder | Complete |
| 9. Embedded documentation | Store the guide source and PDF in `inst/doc` and serve it from the app | The installed package contains both files without external `docs` or `output` copies | Complete |
| 10. Unpacked distribution | Create an accessible copy of the built source package | `package-copy/micsPlusTableR` mirrors the release archive without being compressed | Complete |

## Deferred architectural work

The two established tabulation engines (`tabulate_v()` and `tabulate_h()` +
`calc_cells()`) use different denominator and block models. They will be kept
separate in version 0.2.4 to avoid changing statistical results during the
packaging migration. A later release can replace string evaluation and implicit
engine state with a typed `TabulationContext`, supported by regression fixtures
for representative Jamaica and Mongolia tables.
