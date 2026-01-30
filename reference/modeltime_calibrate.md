# Preparation for forecasting

Calibration sets the stage for accuracy and forecast confidence by
computing predictions and residuals from out of sample data.

## Usage

``` r
modeltime_calibrate(object, new_data, id = NULL, quiet = TRUE, ...)
```

## Arguments

- object:

  A fitted model object that is either:

  1.  A modeltime table that has been created using
      [`modeltime_table()`](https://business-science.github.io/modeltime/reference/modeltime_table.md)

  2.  A workflow that has been fit by `fit.workflow()` or

  3.  A parsnip model that has been fit using
      [`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.html)

- new_data:

  A test data set `tibble` containing future information (timestamps and
  actual values).

- id:

  A quoted column name containing an identifier column identifying time
  series that are grouped.

- quiet:

  Hide errors (`TRUE`, the default), or display them as they occur?

- ...:

  Additional arguments passed to
  [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md).

## Value

A Modeltime Table (`mdl_time_tbl`) with nested `.calibration_data` added

## Details

The results of calibration are used for:

- **Forecast Confidence Interval Estimation**: The out of sample
  residual data is used to calculate the confidence interval. Refer to
  [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md).

- **Accuracy Calculations:** The out of sample actual and prediction
  values are used to calculate performance metrics. Refer to
  [`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md)

The calibration steps include:

1.  If not a Modeltime Table, objects are converted to Modeltime Tables
    internally

2.  Two Columns are added:

- `.type`: Indicates the sample type. This is:

  - "Test" if predicted, or

  - "Fitted" if residuals were stored during modeling.

- `.calibration_data`:

  - Contains a tibble with Timestamps, Actual Values, Predictions and
    Residuals calculated from `new_data` (Test Data)

  - If `id` is provided, will contain a 5th column that is the
    identifier variable.

## Examples

``` r
library(dplyr)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)

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

# ---- CALIBRATE ----

calibration_tbl <- models_tbl %>%
    modeltime_calibrate(
        new_data = testing(splits)
    )

# ---- ACCURACY ----

calibration_tbl %>%
    modeltime_accuracy()
#> # A tibble: 1 × 9
#>   .model_id .model_desc .type   mae  mape  mase smape  rmse   rsq
#>       <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1         1 PROPHET     Test   266.  2.68 0.791  2.62  359. 0.812

# ---- FORECAST ----

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    )
#> # Forecast Results
#>   
#> Conf Method: conformal_default | Conf Interval: 0.95 | Conf By ID: FALSE
#> (GLOBAL CONFIDENCE)
#> # A tibble: 368 × 7
#>    .model_id .model_desc .key   .index     .value .conf_lo .conf_hi
#>        <int> <chr>       <fct>  <date>      <dbl>    <dbl>    <dbl>
#>  1        NA ACTUAL      actual 1990-01-01   6370       NA       NA
#>  2        NA ACTUAL      actual 1990-02-01   6430       NA       NA
#>  3        NA ACTUAL      actual 1990-03-01   6520       NA       NA
#>  4        NA ACTUAL      actual 1990-04-01   6580       NA       NA
#>  5        NA ACTUAL      actual 1990-05-01   6620       NA       NA
#>  6        NA ACTUAL      actual 1990-06-01   6690       NA       NA
#>  7        NA ACTUAL      actual 1990-07-01   6000       NA       NA
#>  8        NA ACTUAL      actual 1990-08-01   5450       NA       NA
#>  9        NA ACTUAL      actual 1990-09-01   6480       NA       NA
#> 10        NA ACTUAL      actual 1990-10-01   6820       NA       NA
#> # ℹ 358 more rows

```
