# Application screenshots

Captured on 8 September 2026 from the installed `micsPlusTableR` 0.2.7
application, GitHub commit `29e158ad20201b0d528d307461ae07f92b5c0fff`.
These are actual Chrome application screenshots, not mockups. They are
embedded in `../user-guide.qmd`; keep them with that source when rendering.

The example uses the Mongolia 2025-26 Wave 1 inputs from the survey workspace
`micsPlusTableRv2`; these data files are not bundled in the package.
Only aggregate results and application controls are shown. The two optional
individual-data/exploration screens were captured without displaying records.
The temporary screenshot run wrote to `/private/tmp/mics-guide-output`, not
the user's production output folder. The guide explicitly explains that
ordinary project runs use `micsPlusTableR-output`.

| File | State shown |
|---|---|
| `01-data-preparation.png` | Inputs selected and survey metadata matched |
| `02-tabulator.png` | Table 1.1a calculated, Formatted/Header preview |
| `03-consistency-checks.png` | Checks run, numeric comparison results |
| `04-write-to-excel.png` | Both workbook destinations created; current table written |
| `05-multi-sheet.png` | Sheets 1.1a and 1.1b written with no consistency issues |
| `06-long-format.png` | Combined data prepared and download buttons available |
| `07-data-view.png` | Controls before displaying individual records |
| `08-data-exploration.png` | Controls before running an exploratory analysis |

To refresh: launch the installed package locally, follow the same guide steps
with approved example inputs, and capture the actual screens. Keep raw
individual records out of the guide. Update the recorded version and commit,
then render and inspect both HTML and PDF outputs. A screenshot of a successful
calculation documents application behavior; it is not statistical approval of
the full survey.
