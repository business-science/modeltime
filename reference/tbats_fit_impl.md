# Low-Level tbats function for translating modeltime to forecast

Low-Level tbats function for translating modeltime to forecast

## Usage

``` r
tbats_fit_impl(
  x,
  y,
  period_1 = "auto",
  period_2 = NULL,
  period_3 = NULL,
  use.parallel = length(y) > 1000,
  ...
)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- period_1:

  (required) First seasonal frequency. Uses "auto" by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided.

- period_2:

  (optional) First seasonal frequency. Uses "auto" by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided.

- period_3:

  (optional) First seasonal frequency. Uses "auto" by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided.

- use.parallel:

  `TRUE/FALSE` indicates whether or not to use parallel processing.

- ...:

  Additional arguments passed to
  [`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html)
