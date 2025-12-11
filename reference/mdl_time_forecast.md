# Modeltime Forecast Helpers

Used for low-level forecasting of modeltime, parnsip and workflow
models. These functions are not intended for user use.

## Usage

``` r
mdl_time_forecast(
  object,
  calibration_data,
  new_data = NULL,
  h = NULL,
  actual_data = NULL,
  bind_actual = TRUE,
  keep_data = FALSE,
  arrange_index = FALSE,
  ...
)
```

## Arguments

- object:

  A Modeltime Table

- calibration_data:

  Data that has been calibrated from a testing set

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

- bind_actual:

  Logical. Whether or not to skip rowwise binding of \`actual_data“

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

A tibble with forecast features
