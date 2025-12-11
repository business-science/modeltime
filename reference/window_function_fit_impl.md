# Low-Level Window Forecast

Low-Level Window Forecast

## Usage

``` r
window_function_fit_impl(
  x,
  y,
  id = NULL,
  window_size = "all",
  window_function = NULL,
  ...
)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- id:

  An optional ID feature to identify different time series. Should be a
  quoted name.

- window_size:

  The period to apply the window function to

- window_function:

  A function to apply to the window. The default is
  [`mean()`](https://rdrr.io/r/base/mean.html).

- ...:

  Additional arguments for the `window_function`. For example, it's
  common to pass `na.rm = TRUE` for the mean forecast.
