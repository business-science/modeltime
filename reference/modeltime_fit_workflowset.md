# Fit a `workflowset` object to one or multiple time series

This is a wrapper for
[`fit()`](https://generics.r-lib.org/reference/fit.html) that takes a
`workflowset` object and fits each model on one or multiple time series
either sequentially or in parallel.

## Usage

``` r
modeltime_fit_workflowset(
  object,
  data,
  ...,
  control = control_fit_workflowset()
)
```

## Arguments

- object:

  A workflow_set object, generated with the workflowsets::workflow_set
  function.

- data:

  A `tibble` that contains data to fit the models.

- ...:

  Not currently used.

- control:

  An object used to modify the fitting process. See
  [`control_fit_workflowset()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).

## Value

A Modeltime Table containing one or more fitted models.

## See also

[`control_fit_workflowset()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)

## Examples

``` r
library(tidymodels)
library(workflowsets)
library(dplyr)
library(lubridate)
library(timetk)

data_set <- m4_monthly

# SETUP WORKFLOWSETS

rec1 <- recipe(value ~ date + id, data_set) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

mod1 <- linear_reg() %>% set_engine("lm")

mod2 <- prophet_reg() %>% set_engine("prophet")

wfsets <- workflowsets::workflow_set(
    preproc = list(rec1 = rec1),
    models  = list(
        mod1 = mod1,
        mod2 = mod2
    ),
    cross   = TRUE
)

# FIT WORKFLOWSETS
# - Returns a Modeltime Table with fitted workflowsets

wfsets %>% modeltime_fit_workflowset(data_set)
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.
#> # Modeltime Table
#> # A tibble: 2 × 3
#>   .model_id .model     .model_desc
#>       <int> <list>     <chr>      
#> 1         1 <workflow> REC1_MOD1  
#> 2         2 <workflow> REC1_MOD2  
```
