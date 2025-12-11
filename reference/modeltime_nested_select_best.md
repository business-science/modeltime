# Select the Best Models from Nested Modeltime Table

Finds the best models for each time series group in a Nested Modeltime
Table using a `metric` that the user specifies.

- Logs the best results, which can be accessed with
  [`extract_nested_best_model_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

- If `filter_test_forecasts = TRUE`, updates the test forecast log,
  which can be accessed
  [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

## Usage

``` r
modeltime_nested_select_best(
  object,
  metric = "rmse",
  minimize = TRUE,
  filter_test_forecasts = TRUE
)
```

## Arguments

- object:

  A Nested Modeltime Table

- metric:

  A metric to minimize or maximize. By default available metrics are:

  - "rmse" (default)

  - "mae"

  - "mape"

  - "mase"

  - "smape"

  - "rsq"

- minimize:

  Whether to minimize or maximize. Default: TRUE (minimize).

- filter_test_forecasts:

  Whether or not to update the test forecast log to filter only the best
  forecasts. Default: TRUE.
