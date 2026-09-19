# micsPlusTableR

`micsPlusTableR` turns a MICS Plus Excel tabulation plan, household/member
survey files, and a survey-specific preparation script into reviewed Excel
tables through a Shiny application.

## Install and run

Start with the [printable User's Guide](inst/doc/user-guide.pdf) for instructions
that assume no previous R experience: installing R and RStudio, creating or
opening a survey project, installing the package, selecting survey files,
checking tables, and saving Excel workbooks. You can read it before installing.

1. Install [R](https://cloud.r-project.org/) (4.2 or later) and
   [RStudio Desktop](https://posit.co/download/rstudio-desktop/). Install R first.
2. In RStudio, create a survey project with **File > New Project > New Directory
   > New Project**. To use an existing folder, choose **Existing Directory**.
   To open an existing `.Rproj` file, use **File > Open Project**. Check that the
   intended survey project name appears at the top right before continuing.
3. In the **R Console**, run these lines one at a time, waiting for `>` between
   them. They install from the [package repository](https://github.com/bariscr/micsPlusTableR):

```r
install.packages("remotes")
remotes::install_github("bariscr/micsPlusTableR")
```

4. Use **Session > Restart R**, then run `packageVersion("micsPlusTableR")` to
   confirm installation. Start the application:

```r
micsPlusTableR::run_app()
```

5. In **Download Files**, load the survey list, choose one, several, or all
   survey folders, and choose a download folder (your current project by default).
   Download the plans and preparation files. Obtain household/member data from
   the [MICS surveys page](https://mics.unicef.org/surveys) with permission.
6. In **Data Preparation**, choose the country/period and wave, use **Browse**
   to select the household and household members `.sav` files, Excel tabulation
   plan, and complete preparation folder, then
   click **Set**. In **Tabulator**, calculate a table; review **Consistency
   Checks**, then use **Write to Excel** to create and fill the output workbooks.

On later visits, reopen the survey project and run `micsPlusTableR::run_app()`.
Select your inputs again; installation is not an everyday step. Keep the R
session and browser open while working. Each user works in their own survey
project; no maintainer workspace or source checkout is needed.

### Other R environments

You can also use **Positron**, **Visual Studio Code**, **Cursor**,
**JupyterLab/Notebook with an R kernel**, or **R's own console/a terminal**.
Install R and any R support needed by the chosen editor. Open your survey
folder, start R there, and run the same commands in R. Check `getwd()` and
`here::here()` before launching so results go to the intended project, or
set `output_dir` explicitly. Use `launch.browser = TRUE` when your environment
does not open the browser automatically. Remote sessions need administrator
setup for browser access and retrieving server-side files. The User's Guide
includes links to editor setup documentation and explains these alternatives.

### Installation options for support staff

The installation workflow checks clean installation and replacement of an
existing copy on macOS, Windows, and Linux. It builds the current checkout
through `remotes`, keeps R's staged installation and load checks enabled, and
uses a disposable library. Maintainers can run the same checks locally with
`Rscript tools/test-installation.R` after installing the package dependencies
and `remotes`.

Install directly from a package source folder with:

```r
remotes::install_local("/path/to/micsPlusTableR")
```

Or install a built source archive using base R:

```r
install.packages(
  "build/micsPlusTableR_0.3.1.tar.gz",
  repos = NULL,
  type = "source"
)
```

At startup, the app uses `here::here()` to find the user's project root and
automatically creates `micsPlusTableR-output` there. This is portable across
Windows, macOS, and Linux. Generated Output and Formatted workbooks are written
there. Choose another default directory when launching:

```r
micsPlusTableR::run_app(output_dir = "path/to/project-output")
```

## Preparation script dependencies

All packages required by the maintained preparation scripts are installed
with `micsPlusTableR` through the normal package installation command. There is
no separate preparation dependency installation step. They are documented
here separately to distinguish their purpose from app and tabulation packages.

The maintained scripts declare `prep_dependencies` near the top of each main R
file. The app checks these requirements automatically before preparation and
reports the package and helper script if a package is missing or cannot load.

Mongolia Waves 1 and 2 use **dplyr** and **haven**, already required by the app.
Jamaica's FIES workflows additionally require **ggplot2**, **Hmisc**, **memisc**,
**reshape2**, **RM.weights**, **survey**, and **srvyr**. Jamaica Wave 2 also uses
**labelled**. Its other shared packages (including **purrr**) are already app
requirements. The checker reads the selected script and its helpers, so use
its results for the exact survey-specific list.

Preparation-only packages are required `Imports`, alongside app dependencies.
The `Config/micsPlusTableR/prep-packages` field identifies them separately for
documentation. `check_prep_dependencies()` and `install_prep_dependencies()`
remain available for diagnosis, repair, or custom scripts with additional
requirements; they are not part of normal setup.
Use `help("prep-script-dependencies", package = "micsPlusTableR")` for their
purposes and script-author instructions, or `help("package-dependencies",
package = "micsPlusTableR")` for the 23 required app/engine packages.

## Maintain survey and wave choices (maintainers)

Edit `inst/extdata/survey_choices.csv` in the
development checkout to change the app's country/period and wave dropdowns.
The CSV has one row per available country, period, and wave combination:

```csv
country,country_code,period,wave
Jamaica,JAM,2023-24,Wave 1
Jamaica,JAM,2023-24,Wave 2
Mongolia,MNG,2025-26,Wave 1
Mongolia,MNG,2025-26,Wave 2
```

Use the full country name, its three-letter uppercase code, the survey period
as text (for example `2025-26`), and the wave label (for example `Wave 3`).
Keep the column names, fill every field, and avoid duplicate combinations.
Use the same country code for all waves of a country/period. Save as UTF-8 CSV.
Row order controls dropdown order; the first country/period and its first wave
are the defaults. Only waves listed for the selected country/period are shown.
The code and period also supply output filenames and long-format metadata.

Restart the app after saving the CSV. With `source("run_dev.R")`, source-folder
edits are picked up directly. For an installed package, reinstall the updated
package folder and restart the app. The installed copy can be located with
`system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")`;
direct edits to that copy are replaced on reinstall or upgrade.

## Download tabulation plans and preparation files

Tabulation plans, preparation scripts, and supporting reference assets are maintained in
[micsPlusTableR-Files](https://github.com/bariscr/micsPlusTableR-Files).
Household and household-member microdata are **not** distributed there or by
these functions. Obtain the `.sav` data files separately from the
[MICS surveys page](https://mics.unicef.org/surveys) with the required permission.
Once the files repository is public, no GitHub account is needed for its
plans and preparation files. You can
[download all plans and preparation files as a ZIP](https://github.com/bariscr/micsPlusTableR-Files/archive/refs/heads/main.zip)
and extract it, use the app tab, or run the Console commands below.

### Download from the application

The app opens on **Data Preparation**. To download files, open **Download Files** and click
**Load / refresh surveys**, then choose **Selected surveys** and tick one or
more folders, or choose **All surveys**. The preview shows the selected file
count and size. **Download folder** defaults to the current project captured
when `run_app()` starts. Each survey is saved in its own subfolder, such as
`<project>/JAM_W1/`.

Use **Browse folders...** or type another path; relative paths are resolved
against the current project. **Use current project** restores the default.
Leave **Replace existing files** off unless you intend to replace local copies.
Click **Download files**, wait for the saved-location message, then open
**Data Preparation** and select the materials together with your separately
authorized household/member data. On a remote R host, these paths refer to
that host's filesystem.

### Download from the R Console

The functions below are an alternative to the app tab. Their default destination
is `inputs/` under `getwd()`; the app tab instead defaults to the project itself.

```r
# See available survey IDs and file paths
available <- micsPlusTableR::list_survey_files()
unique(available$survey)

# Plans and complete preparation folder for one survey
micsPlusTableR::download_survey_files("JAM_W1")

# Several surveys
micsPlusTableR::download_survey_files(c("JAM_W1", "MNG_W2"))

# All published surveys
micsPlusTableR::download_survey_files()

# Individual files: use exact paths from available$path
micsPlusTableR::download_survey_files(
  files = available$path[1],
  dest_dir = "selected-inputs"
)
```

These are alternative examples. Downloads go under `inputs` in the current
working directory (`getwd()`), keeping folders such as `JAM_W1`
and all preparation subfolders. To replace an existing selection deliberately,
pass `overwrite = TRUE`; otherwise an existing file stops the download.
Select a whole survey folder to include all its preparation helpers. These
downloads do not include the household/member data needed to run the app.
Save your separately authorized data alongside the matching plans and scripts.
Downloaded scripts are saved without being executed. Select the files in the app as usual.
New survey folders are discovered from GitHub without updating the package.
Use `ref = "<commit-or-tag>"` to retrieve a recorded version; the returned table's
`commit` attribute records the resolved commit. Internet access and a public
files repository are required. If GitHub limits requests, retry later or use
the ZIP link. Before publication, obtain the plans and preparation files from
your survey lead.

## Required inputs

- An Excel tabulation plan with an `IDX` sheet and one sheet per table.
- Household (`hh`) and household-member (`hl`) SPSS `.sav` files obtained
  separately from the MICS Plus website with permission.
- A selected preparation folder containing one top-level prep-like `.R` script
  that reads `hh_path` and `hl_path` and creates objects named `hh` and `hl`.
- An optional `FIES-inputs` subfolder containing FIES helper code and assets.

The bundled illustrated HTML guide explains application tasks with screenshots,
simple steps, and troubleshooting. Open it from the application's **User's Guide**
tab; a printable PDF is available there too. Package functions and technical
reference material are documented separately below.

## Use the tabulation engine without Shiny

### Decimal places in worksheet statistics

Add an optional `d=` argument to any supported statistic to choose its displayed
number of decimal places. For example:

| Worksheet statistic | Display rule |
|---|---|
| `mean(HCS8, d=0)` | Whole-number mean: `442,572` instead of `442,571.9`. |
| `mean(d=1)` | Mean indicator with one decimal place. |
| `p(d=2)` | Percentage with two decimals, such as `73.30`. |
| `n(d=1)` | Count with one decimal, such as `881.0`. |
| `n_unw(d=0)` | Unweighted count with no decimals. |
| `median(age, d=2)` | Median with two decimals. |
| `p(100, d=2)` | Existing total/distribution statistic with two decimals. |

The argument also works with `mean_unw`, legacy `Mean`, `p_sum`, and other
supported statistics; `100(d=2)` formats a constant. Use the statistic supported
by your table direction. `d` must be a literal non-negative whole number,
supplied once; spaces around arguments are allowed. It specifies decimal places,
not significant figures. **Without `d`, all existing display defaults remain in
effect**, including one decimal for means.

Overrides apply to the Formatted and Suppressed Tabulator views, supplied extra
tables, and both Excel workbooks (single-sheet and multi-sheet). Unformatted
Tabulator values retain their full precision. Excel stores numeric estimates
and applies the requested number format. Parenthesized estimates use the same
decimals; `(*)` and `-` remain unchanged.

`d` does not change calculations, weights, filters, denominators, suppression
thresholds, or consistency checks. A change from `mean(x,d=0)` to `mean(x,d=2)`
is still a mean-to-mean transition. `n_unw(d=2)` is still recognized as the
unweighted count for direction and suppression. Blank statistic cells inherit
precision with the statistic through the existing worksheet-filling rules.
Long-format output records the override in `DECIMALS` and its display values,
while `OBS_VALUE` retains full precision.

### Worksheet filters

The condition-row `filter(...)` in **column B is global**: it applies to every
calculated population, including later horizontal filter blocks. An additional
horizontal `filter(...)` starts at its own column and combines with B. It extends
right within each row until another explicit filter starts or a non-mean
statistic is followed by a mean statistic. That mean cell uses B alone unless
it starts a new filter. An expired filter stays off until an explicit filter
starts. Changes down rows do not stop filters. A local filter over `p` continues
through `n` and `n_unw`, preserving the population used for percentage bases.

Scope uses effective statistics after worksheet filling and trimming surrounding
whitespace. Mean forms are `mean(variable)`, `mean`, and the legacy `Mean` form.
Other statistic changes, including `mean(age)` to `mean(income)` and a mean to
`n`, do not stop inheritance. A later non-mean-to-mean transition still stops it.
Blank filter cells do not reset a filter; `filter(TRUE)` starts an unrestricted
local block while preserving B. Filters are applied before block calculations.
Vertical tables retain their existing first-filter behavior and do not process
additional horizontal filter blocks. The User's Guide section **4A. Understand
worksheet filters** includes a column-by-column example and direction details.

### Scripted workflow

The package also provides a supported scripting API. State is held in an
explicit session rather than the global workspace:

```r
session <- micsPlusTableR::mics_session()

micsPlusTableR::prepare_mics_data(
  session,
  hh_path = "hh.sav",
  hl_path = "hl.sav",
  prep_script = "survey-preparation/survey_prep.R",
  output_dir = here::here("micsPlusTableR-output")
)

result <- micsPlusTableR::tabulate_mics_table(
  session,
  path_tab_excel = "TabulationPlan.xlsx",
  sheet = "1.1"
)

checks <- micsPlusTableR::check_mics_table(session, result)
preview <- micsPlusTableR::pivot_mics_table(session, result, type = "header")
```

Use `write_mics_table()`, `write_mics_footnotes()`,
`compare_mics_tables()`, and `read_previous_mics_table()` for output and
regression review.

The application's **Long Format Data** workflow converts all calculated cells
to a 35-variable SDMX-compatible observation contract adapted from the
`mics-translation` project. The output retains `TABLE_ID`, `OBS_VALUE`,
`DECIMALS`, and `denominator_n` and omits their custom duplicates `table_id`,
`value`, `estimate`, `digits`, and `unw`. The output also omits `unweighted_n`,
which duplicates `denominator_n` for engine cells. Source coordinates remain
in `row_order` and `column_order`; `row_id`, `column_id`, `ROW_ID`, and `COLUMN_ID`
are omitted. These coordinates are project metadata, not predefined
SDMX fields. The final unit is retained in `UNIT_MEASURE`; the optional input
override `unit_measure` is applied internally and omitted from the output.
Use `as_sdmx_compatible()` and
`validate_sdmx_compatible()` when building the same output from scripts.
`TIME_PERIOD` retains the full survey period (for example `2025-26`), and the
generated `SURVEY_ID` includes it, for example `MNG_MICSPLUS_2025-26_W2`.

After preparing the data, use the **Excel (.xlsx)**, **CSV (.csv)**, or
**RDS (.rds)** download buttons in the Long Format Data tab. Each download
contains the full prepared dataset, including rows hidden by preview filters
or pagination. RDS preserves R column types and can be loaded with `readRDS()`.
Filenames follow the input-file convention, preserving the full survey period
and wave from the completed run, followed by `LongFormatData` and the download
date, for example `MNG MICSPlus 2025-26 Wave2_LongFormatData_20260907.csv`.

## Package documentation

- [Reference manual](output/pdf/micsPlusTableR-manual.pdf): CRAN-style package
  overview, function arguments, return values, examples, and survey CSV format.
- [Illustrated user guide (HTML)](inst/doc/user-guide.html): everyday application
  setup and tasks with beginner instructions and expandable screenshots. Open the app
  and click **User's Guide** to read it; GitHub displays HTML as source.
- [Printable user guide (PDF)](inst/doc/user-guide.pdf): the same application guide.
- [Manager and support notes](inst/doc/manager-notes.md): preparation-package
  requirements and dependency troubleshooting.
- [Getting started](vignettes/getting-started.Rmd): editable package vignette.

After installation, open the documentation in R:

```r
help(package = "micsPlusTableR")
help("micsPlusTableR-package", package = "micsPlusTableR")
help("survey_choices", package = "micsPlusTableR")
vignette("getting-started", package = "micsPlusTableR")
```

To install the vignette from GitHub, add `build_vignettes = TRUE` to the
`remotes::install_github()` call.

## Development and GitHub

Version `0.3.0` marks the changed workflow and separation of this package from
the previous project. The versioning policy, release checklist, and planned
`1.0.0` milestone are maintained in the maintainer-only
`micsPlusTableR-manager/docs/setup-and-maintenance.md`. That private management
project is not distributed to users; user setup and operating instructions
are maintained here in the package documentation. See [NEWS.md](NEWS.md) for package release notes.

This folder is the package source and GitHub repository root.
`DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, and `inst/` stay at the top level.
Edit these files directly; no conversion is needed before pushing to GitHub.
The editable `inst/extdata/survey_choices.csv` is included automatically in
source builds and installed as `extdata/survey_choices.csv`.

Open `micsPlusTableR.Rproj`, or set your R working directory to this folder.
Install development dependencies once:

```r
install.packages(c("devtools", "pkgload", "roxygen2", "rmarkdown"))
remotes::install_deps(".", dependencies = TRUE)
```

Launch the current source without installing it:

```r
source("run_dev.R")
```

Stop the app, save edits, and run the launcher again to reload the source CSV
and R code. For an installed copy, reinstall and restart instead.

Validate, test, and build from the package root:

```r
pkgload::load_all(".")
micsPlusTableR:::read_survey_choices() # validate and preview the saved CSV
devtools::test(".")
devtools::check(".")
```

```sh
Rscript tools/build-guide.R
Rscript tools/build-package.R
Rscript tools/build-manual.R
```

The guide command renders `inst/doc/user-guide.qmd` to self-contained HTML and
printable PDF; it requires Quarto and XeLaTeX. Keep the source, CSS, screenshots,
and both rendered guides together in `inst/doc/`, and commit the outputs so
users do not need Quarto or LaTeX. Review both formats after edits. The HTML
guide is served directly by Shiny and works without a browser PDF plugin.

The package command validates the CSV and builds a standard source archive in
`build/`, including the vignette. The manual command regenerates the PDF reference
manual from `man/*.Rd`; it requires a working LaTeX installation. Edit function
documentation in its roxygen comments in `R/`, then run
`roxygen2::roxygenise(".", roclets = "rd")`. Handwritten help topics in `man/`
are maintained directly.

Commit the source files, help files, vignette, and reference PDF to GitHub.
Build archives, check output, local R state, and generated survey output are
excluded by `.gitignore`; `.Rbuildignore` keeps development artifacts out of
the installable package. Before publishing, replace the placeholder maintainer
name/email in `DESCRIPTION` with the real project details. Publishing on GitHub
does not require CRAN submission.

The standard directory layout and reference manual follow R's
[Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html).

### Test individual tabulation steps

Use `tabulate_v(session)` or `tabulate_h(session)` for a loaded plan,
`calc_cells()` for explicit cell calculations, and the six individual
`*_total_check()` helpers to investigate one consistency check. Parsing and
cleaning helpers are also exported. See the getting-started vignette and
`help(package = "micsPlusTableR")` for arguments and examples. The direction
functions replace the old names ending in `2`. Calculation errors now include
the worksheet and relevant block, cell, or expression when available.
