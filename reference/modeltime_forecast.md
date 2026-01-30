# Forecast future data

The goal of `modeltime_forecast()` is to simplify the process of
forecasting future data.

## Usage

``` r
modeltime_forecast(
  object,
  new_data = NULL,
  h = NULL,
  actual_data = NULL,
  conf_interval = 0.95,
  conf_by_id = FALSE,
  conf_method = "conformal_default",
  keep_data = FALSE,
  arrange_index = FALSE,
  ...
)
```

## Arguments

- object:

  A Modeltime Table

- new_data:

  A `tibble` containing future information to forecast. If `NULL`,
  forecasts the calibration data.

- h:

  The forecast horizon (can be used instead of `new_data` for time
  series with no exogenous regressors). Extends the calibration data `h`
  periods into the future.

- actual_data:

  Reference data that is combined with the output tibble and given a
  `.key = "actual"`

- conf_interval:

  An estimated confidence interval based on the calibration data. This
  is designed to estimate future confidence from *out-of-sample
  prediction error*.

- conf_by_id:

  Whether or not to produce confidence interval estimates by an ID
  feature.

  - When `FALSE`, a global model confidence interval is provided.

  - If `TRUE`, a local confidence interval is provided group-wise for
    each time series ID. To enable local confidence interval, an `id`
    must be provided during
    [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md).

- conf_method:

  Algorithm used to produce confidence intervals. All CI's are Conformal
  Predictions. Choose one of:

  - `conformal_default`: Uses
    [`qnorm()`](https://rdrr.io/r/stats/Normal.html) to compute
    quantiles from out-of-sample (test set) residuals.

  - `conformal_split`: Uses the split method split conformal inference
    method described by Lei *et al* (2018)

- keep_data:

  Whether or not to keep the `new_data` and `actual_data` as extra
  columns in the results. This can be useful if there is an important
  feature in the `new_data` and `actual_data` needed when forecasting.
  Default: `FALSE`.

- arrange_index:

  Whether or not to sort the index in rowwise chronological order
  (oldest to newest) or to keep the original order of the data. Default:
  `FALSE`.

- ...:

  Not currently used

## Value

A tibble with predictions and time-stamp data. For ease of plotting and
calculations, the column names are transformed to:

- `.key`: Values labeled either "prediction" or "actual"

- `.index`: The timestamp index.

- `.value`: The value being forecasted.

Additionally, if the Modeltime Table has been previously calibrated
using
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md),
you will gain confidence intervals.

- `.conf_lo`: The lower limit of the confidence interval.

- `.conf_hi`: The upper limit of the confidence interval.

Additional descriptive columns are included:

- `.model_id`: Model ID from the Modeltime Table

- `.model_desc`: Model Description from the Modeltime Table

Unnecessary columns are *dropped* to save space:

- `.model`

- `.calibration_data`

## Details

The `modeltime_forecast()` function prepares a forecast for
visualization with with
[`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md).
The forecast is controlled by `new_data` or `h`, which can be combined
with existing data (controlled by `actual_data`). Confidence intervals
are included if the incoming Modeltime Table has been calibrated using
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md).
Otherwise confidence intervals are not estimated.

**New Data**

When forecasting you can specify future data using `new_data`. This is a
future tibble with date column and columns for xregs extending the
trained dates and exogonous regressors (xregs) if used.

- **Forecasting Evaluation Data**: By default, the `new_data` will use
  the `.calibration_data` if `new_data` is not provided. This is the
  equivalent of using
  [`rsample::testing()`](https://rsample.tidymodels.org/reference/initial_split.html)
  for getting test data sets.

- **Forecasting Future Data**: See
  [`timetk::future_frame()`](https://business-science.github.io/timetk/reference/future_frame.html)
  for creating future tibbles.

- **Xregs**: Can be used with this method

**H (Horizon)**

When forecasting, you can specify `h`. This is a phrase like "1 year",
which extends the `.calibration_data` (1st priority) or the
`actual_data` (2nd priority) into the future.

- **Forecasting Future Data**: All forecasts using `h` are ***extended
  after the calibration data or actual_data***.

- Extending `.calibration_data` - Calibration data is given 1st
  priority, which is desirable *after refitting* with
  [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md).
  Internally, a call is made to
  [`timetk::future_frame()`](https://business-science.github.io/timetk/reference/future_frame.html)
  to expedite creating new data using the date feature.

- Extending `actual_data` - If `h` is provided, and the modeltime table
  has not been calibrated, the "actual_data" will be extended into the
  future. This is useful in situations where you want to go directly
  from
  [`modeltime_table()`](https://business-science.github.io/modeltime/reference/modeltime_table.md)
  to `modeltime_forecast()` without calibrating or refitting.

- **Xregs**: Cannot be used because future data must include new xregs.
  If xregs are desired, build a future data frame and use `new_data`.

**Actual Data**

This is reference data that contains the true values of the time-stamp
data. It helps in visualizing the performance of the forecast vs the
actual data.

When `h` is used and the Modeltime Table has *not been calibrated*, then
the actual data is extended into the future periods that are defined by
`h`.

**Confidence Interval Estimation**

Confidence intervals (`.conf_lo`, `.conf_hi`) are estimated based on the
normal estimation of the testing errors (out of sample) from
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md).
The out-of-sample error estimates are then carried through and applied
to applied to any future forecasts.

The confidence interval can be adjusted with the `conf_interval`
parameter. The algorithm used to produce confidence intervals can be
changed with the `conf_method` parameter.

*Conformal Default Method:*

When `conf_method = "conformal_default"` (default), this method uses
[`qnorm()`](https://rdrr.io/r/stats/Normal.html) to produce a 95%
confidence interval by default. It estimates a normal (Gaussian
distribution) based on the out-of-sample errors (residuals).

The confidence interval is *mean-adjusted*, meaning that if the mean of
the residuals is non-zero, the confidence interval is adjusted to widen
the interval to capture the difference in means.

*Conformal Split Method:*

When `conf_method = "conformal_split`, this method uses the split
conformal inference method described by Lei *et al* (2018). This is also
implemented in the `probably` R package's `int_conformal_split()`
function.

*What happens to the confidence interval after refitting models?*

Refitting has no affect on the confidence interval since this is
calculated independently of the refitted model. New observations
typically improve future accuracy, which in most cases makes the
out-of-sample confidence intervals conservative.

**Keep Data**

Include the new data (and actual data) as extra columns with the results
of the model forecasts. This can be helpful when the new data includes
information useful to the forecasts. An example is when forecasting
*Panel Data* and the new data contains ID features related to the time
series group that the forecast belongs to.

**Arrange Index**

By default, `modeltime_forecast()` keeps the original order of the data.
If desired, the user can sort the output by `.key`, `.model_id` and
`.index`.

## References

Lei, Jing, et al. "Distribution-free predictive inference for
regression." *Journal of the American Statistical Association* 113.523
(2018): 1094-1111.

## Examples

``` r
library(dplyr)
library(timetk)
library(parsnip)
library(rsample)

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

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
    modeltime_calibrate(new_data = testing(splits))

# ---- ACCURACY ----

calibration_tbl %>%
    modeltime_accuracy()
#> # A tibble: 1 × 9
#>   .model_id .model_desc .type   mae  mape  mase smape  rmse   rsq
#>       <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1         1 PROPHET     Test   175.  1.67 0.596  1.67  232. 0.881

# ---- FUTURE FORECAST ----

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    )
#> # Forecast Results
#>   
#> Conf Method: conformal_default | Conf Interval: 0.95 | Conf By ID: FALSE
#> (GLOBAL CONFIDENCE)
#> # A tibble: 337 × 7
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
#> # ℹ 327 more rows

# ---- ALTERNATIVE: FORECAST WITHOUT CONFIDENCE INTERVALS ----
# Skips Calibration Step, No Confidence Intervals

models_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    )
#> # Forecast Results
#>   
#> Conf Method: conformal_default | Conf Interval: | Conf By ID: FALSE (GLOBAL
#> CONFIDENCE)
#> # A tibble: 337 × 5
#>    .model_id .model_desc .key   .index     .value
#>        <int> <chr>       <fct>  <date>      <dbl>
#>  1        NA ACTUAL      actual 1990-01-01   6370
#>  2        NA ACTUAL      actual 1990-02-01   6430
#>  3        NA ACTUAL      actual 1990-03-01   6520
#>  4        NA ACTUAL      actual 1990-04-01   6580
#>  5        NA ACTUAL      actual 1990-05-01   6620
#>  6        NA ACTUAL      actual 1990-06-01   6690
#>  7        NA ACTUAL      actual 1990-07-01   6000
#>  8        NA ACTUAL      actual 1990-08-01   5450
#>  9        NA ACTUAL      actual 1990-09-01   6480
#> 10        NA ACTUAL      actual 1990-10-01   6820
#> # ℹ 327 more rows

# ---- KEEP NEW DATA WITH FORECAST ----
# Keeps the new data. Useful if new data has information
#  like ID features that should be kept with the forecast data

calibration_tbl %>%
    modeltime_forecast(
        new_data      = testing(splits),
        keep_data     = TRUE
    )
#> # Forecast Results
#>   
#> Conf Method: conformal_default | Conf Interval: 0.95 | Conf By ID: FALSE
#> (GLOBAL CONFIDENCE)
#> # A tibble: 31 × 10
#>    .model_id .model_desc .key       .index     .value .conf_lo .conf_hi id   
#>        <int> <chr>       <fct>      <date>      <dbl>    <dbl>    <dbl> <fct>
#>  1         1 PROPHET     prediction 2012-12-01 10600.   10141.   11059. M750 
#>  2         1 PROPHET     prediction 2013-01-01 10512.   10054.   10971. M750 
#>  3         1 PROPHET     prediction 2013-02-01 10553.   10094.   11012. M750 
#>  4         1 PROPHET     prediction 2013-03-01 10691.   10232.   11150. M750 
#>  5         1 PROPHET     prediction 2013-04-01 10720.   10261.   11179. M750 
#>  6         1 PROPHET     prediction 2013-05-01 10722.   10263.   11180. M750 
#>  7         1 PROPHET     prediction 2013-06-01 10562.   10104.   11021. M750 
#>  8         1 PROPHET     prediction 2013-07-01  9684.    9226.   10143. M750 
#>  9         1 PROPHET     prediction 2013-08-01  9536.    9077.    9995. M750 
#> 10         1 PROPHET     prediction 2013-09-01 10165.    9706.   10623. M750 
#> # ℹ 21 more rows
#> # ℹ 2 more variables: date <date>, value <dbl>
```
