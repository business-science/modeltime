# Low-Level Temporaral Hierarchical function for translating modeltime to forecast

Low-Level Temporaral Hierarchical function for translating modeltime to
forecast

## Usage

``` r
temporal_hier_fit_impl(
  x,
  y,
  period = "auto",
  comb = c("struc", "mse", "ols", "bu", "shr", "sam"),
  usemodel = c("ets", "arima", "theta", "naive", "snaive"),
  ...
)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- period:

  A seasonal frequency. Uses "auto" by default. A character phrase of
  "auto" or time-based phrase of "2 weeks" can be used if a date or
  date-time variable is provided.

- comb:

  Combination method of temporal hierarchies

- usemodel:

  Model used for forecasting each aggregation level

- ...:

  Additional arguments passed to
  [`forecast::ets`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
