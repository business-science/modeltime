# Extracts modeltime residuals data from a Modeltime Model

If a modeltime model contains `data` with residuals information, this
function will extract the data frame.

## Usage

``` r
pull_modeltime_residuals(object)
```

## Arguments

- object:

  A fitted `parsnip` / `modeltime` model or `workflow`

## Value

A `tibble` containing the model timestamp, actual, fitted, and residuals
data
