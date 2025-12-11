# Modeltime Nested Forecast

Make a new forecast from a Nested Modeltime Table.

## Usage

``` r
modeltime_nested_forecast(
  object,
  h = NULL,
  include_actual = TRUE,
  conf_interval = 0.95,
  conf_method = "conformal_default",
  id_subset = NULL,
  control = control_nested_forecast()
)
```

## Arguments

- object:

  A Nested Modeltime Table

- h:

  The forecast horizon. Extends the "trained on" data "h" periods into
  the future.

- include_actual:

  Whether or not to include the ".actual_data" as part of the forecast.
  If FALSE, just returns the forecast predictions.

- conf_interval:

  An estimated confidence interval based on the calibration data. This
  is designed to estimate future confidence from *out-of-sample
  prediction error*.

- conf_method:

  Algorithm used to produce confidence intervals. All CI's are Conformal
  Predictions. Choose one of:

  - `conformal_default`: Uses
    [`qnorm()`](https://rdrr.io/r/stats/Normal.html) to compute
    quantiles from out-of-sample (test set) residuals.

  - `conformal_split`: Uses the split method split conformal inference
    method described by Lei *et al* (2018)

- id_subset:

  A sequence of ID's from the modeltime table to subset the forecasting
  process. This can speed forecasts up.

- control:

  Used to control verbosity and parallel processing. See
  [`control_nested_forecast()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).

## Details

This function is designed to help users that want to make new forecasts
other than those that are created during the logging process as part of
the Nested Modeltime Workflow.

### Logged Forecasts

The logged forecasts can be extracted using:

- [`extract_nested_future_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md):
  Extracts the future forecast created after refitting with
  [`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md).

- [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md):
  Extracts the test forecast created after initial fitting with
  [`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md).

The problem is that these forecasts are static. The user would need to
redo the fitting, model selection, and refitting process to obtain new
forecasts. This is why `modeltime_nested_forecast()` exists. So you can
create a new forecast without retraining any models.

### Nested Forecasts

The main arguments is `h`, which is a horizon that specifies how far
into the future to make the new forecast.

- If `h = NULL`, a logged forecast will be returned

- If `h = 12`, a new forecast will be generated that extends each series
  12-periods into the future.

- If `h = "2 years"`, a new forecast will be generated that extends each
  series 2-years into the future.

Use the `id_subset` to filter the Nested Modeltime Table `object` to
just the time series of interest.

Use the `conf_interval` to override the logged confidence interval. Note
that this will have no effect if `h = NULL` as logged forecasts are
returned. So be sure to provide `h` if you want to update the confidence
interval.

Use the `control` argument to apply verbosity during the forecasting
process and to run forecasts in parallel. Generally, parallel is better
if many forecasts are being generated.
