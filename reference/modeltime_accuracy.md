# Calculate Accuracy Metrics

This is a wrapper for `yardstick` that simplifies time series regression
accuracy metric calculations from a fitted `workflow` (trained workflow)
or `model_fit` (trained parsnip model).

## Usage

``` r
modeltime_accuracy(
  object,
  new_data = NULL,
  metric_set = default_forecast_accuracy_metric_set(),
  acc_by_id = FALSE,
  quiet = TRUE,
  ...
)
```

## Arguments

- object:

  A Modeltime Table

- new_data:

  A `tibble` to predict and calculate residuals on. If provided,
  overrides any calibration data.

- metric_set:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  that is used to summarize one or more forecast accuracy (regression)
  metrics.

- acc_by_id:

  Should a global or local model accuracy be produced? (Default: FALSE)

  - When `FALSE`, a global model accuracy is provided.

  - If `TRUE`, a local accuracy is provided group-wise for each time
    series ID. To enable local accuracy, an `id` must be provided during
    [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md).

- quiet:

  Hide errors (`TRUE`, the default), or display them as they occur?

- ...:

  If `new_data` is provided, these parameters are passed to
  [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md)

## Value

A tibble with accuracy estimates.

## Details

The following accuracy metrics are included by default via
[`default_forecast_accuracy_metric_set()`](https://business-science.github.io/modeltime/reference/metric_sets.md):

- MAE - Mean absolute error,
  [`mae()`](https://yardstick.tidymodels.org/reference/mae.html)

- MAPE - Mean absolute percentage error,
  [`mape()`](https://yardstick.tidymodels.org/reference/mape.html)

- MASE - Mean absolute scaled error,
  [`mase()`](https://yardstick.tidymodels.org/reference/mase.html)

- SMAPE - Symmetric mean absolute percentage error,
  [`smape()`](https://yardstick.tidymodels.org/reference/smape.html)

- RMSE - Root mean squared error,
  [`rmse()`](https://yardstick.tidymodels.org/reference/rmse.html)

- RSQ - R-squared,
  [`rsq()`](https://yardstick.tidymodels.org/reference/rsq.html)

## Examples

``` r
library(tidymodels)
library(dplyr)
library(lubridate)
library(timetk)


# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# --- MODELS ---

# Model 1: prophet ----
model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.


# ---- MODELTIME TABLE ----

models_tbl <- modeltime_table(
    model_fit_prophet
)

# ---- ACCURACY ----

models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_accuracy(
        metric_set = metric_set(mae, rmse, rsq)
    )
#> # A tibble: 1 × 6
#>   .model_id .model_desc .type   mae  rmse   rsq
#>       <int> <chr>       <chr> <dbl> <dbl> <dbl>
#> 1         1 PROPHET     Test   263.  356. 0.813

```
