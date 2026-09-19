# Notes for survey managers and support staff

## Preparation script dependencies

Installing **micsPlusTableR** also installs the packages required by the
maintained survey preparation scripts. Users do not need a separate installation
step before preparing data or when switching between supported surveys.

The preparation-package details formerly included in the User's Guide are
collected here for managers and support staff:

| Preparation workflow | Packages |
|---|---|
| Mongolia Waves 1 and 2 | `dplyr` and `haven`, also used by the app. |
| Jamaica FIES preparation | `ggplot2`, `Hmisc`, `memisc`, `reshape2`, `RM.weights`, `survey`, and `srvyr`. Wave 2 also uses `labelled`. |

All listed packages are required package dependencies and install with
micsPlusTableR. Jamaica scripts also use shared packages already required by the
app: `here`, `tibble`, and `tidyr`; Wave 2 additionally uses `purrr`.

Before executing a preparation script, the app checks its declared requirements
and detected helper dependencies. If a package is missing or cannot load, the
error identifies the package and script and includes a repair command. Users
should pass this message to their support person.

For diagnosis, use `check_prep_dependencies()`. For an incomplete installation
or extra requirements in a custom script, use `install_prep_dependencies()`.
These functions are not additional steps in the normal user workflow.

For package purposes and maintenance details, open the package reference in R:

```r
help("prep-script-dependencies", package = "micsPlusTableR")
```

Keep dependency declarations current when changing preparation scripts or their
helpers. Dependency checks do not validate statistical results or certify
compatibility with every package version.
