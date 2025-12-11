# Log Extractor Functions for Modeltime Nested Tables

Extract logged information calculated during the
[`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md),
[`modeltime_nested_select_best()`](https://business-science.github.io/modeltime/reference/modeltime_nested_select_best.md),
and
[`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md)
processes.

## Usage

``` r
extract_nested_test_accuracy(object)

extract_nested_test_forecast(object, .include_actual = TRUE, .id_subset = NULL)

extract_nested_error_report(object)

extract_nested_best_model_report(object)

extract_nested_future_forecast(
  object,
  .include_actual = TRUE,
  .id_subset = NULL
)

extract_nested_modeltime_table(object, .row_id = 1)

extract_nested_train_split(object, .row_id = 1)

extract_nested_test_split(object, .row_id = 1)
```

## Arguments

- object:

  A nested modeltime table

- .include_actual:

  Whether or not to include the actual data in the extracted forecast.
  Default: TRUE.

- .id_subset:

  Can supply a vector of id's to extract forcasts for one or more id's,
  rather than extracting all forecasts. If `NULL`, extracts forecasts
  for all id's.

- .row_id:

  The row number to extract from the nested data.
