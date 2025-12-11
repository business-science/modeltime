# Fit Tidymodels Workflows to Nested Time Series

Fits one or more `tidymodels` workflow objects to nested time series
data using the following process:

1.  Models are iteratively fit to training splits.

2.  Accuracy is calculated on testing splits and is logged. Accuracy
    results can be retrieved with
    [`extract_nested_test_accuracy()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

3.  Any model that returns an error is logged. Error logs can be
    retrieved with
    [`extract_nested_error_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

4.  Forecast is predicted on testing splits and is logged. Forecast
    results can be retrieved with
    [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

## Usage

``` r
modeltime_nested_fit(
  nested_data,
  ...,
  model_list = NULL,
  metric_set = default_forecast_accuracy_metric_set(),
  conf_interval = 0.95,
  conf_method = "conformal_default",
  control = control_nested_fit()
)
```

## Arguments

- nested_data:

  Nested time series data

- ...:

  Tidymodels `workflow` objects that will be fit to the nested time
  series data.

- model_list:

  Optionally, a [`list()`](https://rdrr.io/r/base/list.html) of
  Tidymodels `workflow` objects can be provided

- metric_set:

  A
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  that is used to summarize one or more forecast accuracy (regression)
  metrics.

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

- control:

  Used to control verbosity and parallel processing. See
  [`control_nested_fit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).

## Details

### Preparing Data for Nested Forecasting

Use
[`extend_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md),
[`nest_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md),
and
[`split_nested_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md)
for preparing data for Nested Forecasting. The structure must be a
nested data frame, which is suppplied in
`modeltime_nested_fit(nested_data)`.

### Fitting Models

Models must be in the form of `tidymodels workflow` objects. The models
can be provided in two ways:

1.  Using `...` (dots): The workflow objects can be provided as dots.

2.  Using `model_list` parameter: You can supply one or more workflow
    objects that are wrapped in a
    [`list()`](https://rdrr.io/r/base/list.html).

### Controlling the fitting process

A `control` object can be provided during fitting to adjust the
verbosity and parallel processing. See
[`control_nested_fit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).
