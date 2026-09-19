# MICS Plus Tabulation — micsPlusTableR

**MICS Plus Tabulation** is a system for turning MICS Plus survey data into
statistical tables, checking the results, and exploring the underlying data.
It connects a tabulation plan with the data and preparation steps needed to
calculate the tables. You can review results in the app, save Excel workbooks,
or download combined table results for further use.

If you have arrived from the MICS Plus website, this is the home of the
**micsPlusTableR R package**, which provides the application and its calculation
engine. Install the package on your computer, then launch the app from R or
RStudio. The app opens in a browser while R performs the calculations.

**Start with the [User's Guide (PDF)](inst/doc/user-guide.pdf)** for illustrated,
step-by-step instructions. You can read it before installing anything.

## How the system fits together

| Part | Role |
|---|---|
| **Survey data** | Household and household-member SPSS (`.sav`) files contain the records to analyse. Obtain the matching data separately through MICS with the required access permission. |
| **Tabulation plan (Excel)** | Defines the tables: datasets, row and column groups, filters, conditions, statistics, weights, and display settings. |
| **Preparation folder** | Contains the survey-specific R script and supporting files that prepare the data and variables used by the plan. |
| **micsPlusTableR package and app** | Bring these inputs together to prepare data, calculate and check tables, explore records, and produce outputs. |

The plan, preparation files, and data must match the survey and wave you want
to analyse. Supplied plans and preparation folders are available through the
app's **Download Files** tab and the
[micsPlusTableR-Files repository](https://github.com/bariscr/micsPlusTableR-Files).
Survey microdata are obtained separately from the
[MICS surveys page](https://mics.unicef.org/surveys).

The package was created primarily to serve the app. Most other functions
perform background tasks or expose its calculation engine. Their documentation
supports understanding the system, testing, and working directly in R; app
users do not need to call these functions individually.

## What you can do

- **Use a supplied tabulation plan** with its matching preparation files and
  data. You can run it through the app without writing calculations or changing
  code.
- **Create or adapt a tabulation plan** for your own tables, then apply it with
  compatible preparation files and data. The guide explains the worksheet
  structure and calculation rules for both interpreting and writing plans.

Either route lets you calculate individual tables, review consistency checks,
run several tables together, inspect prepared records, and explore frequencies
and cross-tabulations. You can keep your review in the app or save results as
Excel table workbooks. Combined calculated table cells can also be downloaded
as Excel, CSV, or RDS files. Exporting is a choice, not a required final step.

## Get started

1. **Set up your computer and survey project.** Install
   [R](https://cloud.r-project.org/) (4.2 or later), then
   [RStudio Desktop](https://posit.co/download/rstudio-desktop/). In RStudio,
   use **File > New Project** to create a project for your survey work.
   Open an existing project by double-clicking its `.Rproj` file or choosing
   **File > Open Project**. The user's guide walks through each step.

2. **Install the package.** Run the following commands in RStudio's R Console,
   one at a time. Install `remotes` only if it is not already available; once
   installed, you do not need to install it again for ordinary use or package
   updates. The second command installs or updates `micsPlusTableR` and its
   required dependencies.

   ```r
   install.packages("remotes")
   remotes::install_github("bariscr/micsPlusTableR")
   ```

3. **Restart R and open the app.** Use **Session > Restart R** after installation
   or an update, then run:

   ```r
   micsPlusTableR::run_app()
   ```

4. **Get the matching survey inputs.** In **Download Files**, load the survey
   list and download the plan and complete preparation folder for your survey
   and wave. Obtain the household and household-member data separately. If you
   already have these inputs, use your existing files.

5. **Prepare and review your tables.** In **Data Preparation**, select the survey
   and wave, upload the two data files, tabulation plan, and preparation folder,
   then click **Set**. Open **Tabulator** to calculate a table and
   **Consistency Checks** to inspect applicable checks. Use **Write to Excel**
   when you want to save a table, or **Multi-Sheet Tabulator** to calculate and
   write several sheets together.

For later visits, reopen your survey project and run
`micsPlusTableR::run_app()`, then select your inputs for the new session. You do
not need to repeat installation. Keep R and the browser open while using the app.

Generated workbooks default to `micsPlusTableR-output` in your survey project.
Multi-Sheet Tabulator creates workbooks automatically when destinations have
not been set; for single-table writing, first create or select them in
**Write to Excel**. To continue a saved workbook after restarting the app,
select that workbook again.

Installation and downloads need internet. The workbook preview also loads an
external browser library; caching may allow later reuse but is not guaranteed.
With packages and inputs available locally, table calculations and Excel
output do not need that download.

## Guides and reference

| Resource | Use it for |
|---|---|
| [User's Guide (PDF)](inst/doc/user-guide.pdf) | Setup, the app's tools in navigation order, understanding and creating tabulation plans, and troubleshooting. Also available inside the app's **User's Guide** tab. |
| [Package reference manual](output/pdf/micsPlusTableR-manual.pdf) | How the supporting functions work, their arguments and examples, statistic types, filters, and display precision. |
| [Getting-started vignette](vignettes/getting-started.Rmd) | Further workflow details and examples for working directly in R. |
| [Preparation dependency notes](inst/doc/manager-notes.md) | Diagnosing preparation-package requirements and installation problems. |
| [Release notes](NEWS.md) | Changes between package versions. |

RStudio is the setup used in the steps above. The user's guide also explains
other R environments. After installation, use
`help(package = "micsPlusTableR")` to open the function reference in R.

## Development and maintenance

The following notes are for people modifying the package. Ordinary app setup
is covered above and in the user's guide.

<details>
<summary>Package development, builds, and tests</summary>

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

### Installation checks


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

</details>

<details>
<summary>Maintain survey and wave choices</summary>


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
package folder and restart the R session before launching the app. The installed copy can be located with
`system.file("extdata", "survey_choices.csv", package = "micsPlusTableR")`;
direct edits to that copy are replaced on reinstall or upgrade.

</details>
