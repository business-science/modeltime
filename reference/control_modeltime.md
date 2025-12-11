# Control aspects of the training process

These functions are matched to the associated training functions:

- `control_refit()`: Used with
  [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md)

- `control_fit_workflowset()`: Used with
  [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md)

- `control_nested_fit()`: Used with
  [`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md)

- `control_nested_refit()`: Used with
  [`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md)

- `control_nested_forecast()`: Used with
  [`modeltime_nested_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_nested_forecast.md)

## Usage

``` r
control_refit(verbose = FALSE, allow_par = FALSE, cores = 1, packages = NULL)

control_fit_workflowset(
  verbose = FALSE,
  allow_par = FALSE,
  cores = 1,
  packages = NULL
)

control_nested_fit(
  verbose = FALSE,
  allow_par = FALSE,
  cores = 1,
  packages = NULL
)

control_nested_refit(
  verbose = FALSE,
  allow_par = FALSE,
  cores = 1,
  packages = NULL
)

control_nested_forecast(
  verbose = FALSE,
  allow_par = FALSE,
  cores = 1,
  packages = NULL
)
```

## Arguments

- verbose:

  Logical to control printing.

- allow_par:

  Logical to allow parallel computation. Default: `FALSE` (single
  threaded).

- cores:

  Number of cores for computation. If -1, uses all available physical
  cores. Default: `1`.

- packages:

  An optional character string of additional R package names that should
  be loaded during parallel processing.

  - Packages in your namespace are loaded by default

  - Key Packages are loaded by default: `tidymodels`, `parsnip`,
    `modeltime`, `dplyr`, `stats`, `lubridate` and `timetk`.

## Value

A List with the control settings.

## See also

- Setting Up Parallel Processing:
  [`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md),
  \[parallel_stop())\]

- Training Functions: \[modeltime_refit()\],
  \[modeltime_fit_workflowset()\], \[modeltime_nested_fit()\],
  \[modeltime_nested_refit()\]

\[parallel_stop())\]: R:parallel_stop()) \[modeltime_refit()\]:
R:modeltime_refit() \[modeltime_fit_workflowset()\]:
R:modeltime_fit_workflowset() \[modeltime_nested_fit()\]:
R:modeltime_nested_fit() \[modeltime_nested_refit()\]:
R:modeltime_nested_refit()

## Examples

``` r
# No parallel processing by default
control_refit()
#> refit control object
#> --------------------
#> allow_par : FALSE 
#> cores     : 1 
#> verbose   : FALSE 

# Allow parallel processing and use all cores
control_refit(allow_par = TRUE, cores = -1)
#> refit control object
#> --------------------
#> allow_par : TRUE 
#> cores     : 4 
#> verbose   : FALSE 
#> packages  : modeltime parsnip workflows dplyr stats lubridate tidymodels timetk rsample recipes yardstick dials tune workflowsets tidyr tailor purrr modeldata infer ggplot2 scales broom smooth greybox graphics grDevices utils datasets methods base 

# Set verbosity to show additional training information
control_refit(verbose = TRUE)
#> refit control object
#> --------------------
#> allow_par : FALSE 
#> cores     : 1 
#> verbose   : TRUE 

# Add additional packages used during modeling in parallel processing
# - This is useful if your namespace does not load all needed packages
#   to run models.
# - An example is if I use `temporal_hierarchy()`, which depends on the `thief` package
control_refit(allow_par = TRUE, packages = "thief")
#> refit control object
#> --------------------
#> allow_par : TRUE 
#> cores     : 1 
#> verbose   : FALSE 
#> packages  : modeltime parsnip workflows dplyr stats lubridate tidymodels timetk rsample recipes yardstick dials tune workflowsets tidyr tailor purrr modeldata infer ggplot2 scales broom smooth greybox graphics grDevices utils datasets methods base thief 
```
