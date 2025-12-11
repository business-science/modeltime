# Low-Level ARIMA function for translating modeltime to forecast

Low-Level ARIMA function for translating modeltime to forecast

## Usage

``` r
auto_arima_fit_impl(
  x,
  y,
  period = "auto",
  max.p = 5,
  max.d = 2,
  max.q = 5,
  max.P = 2,
  max.D = 1,
  max.Q = 2,
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

- max.p:

  The maximum order of the non-seasonal auto-regressive (AR) terms.

- max.d:

  The maximum order of integration for non-seasonal differencing.

- max.q:

  The maximum order of the non-seasonal moving average (MA) terms.

- max.P:

  The maximum order of the seasonal auto-regressive (SAR) terms.

- max.D:

  The maximum order of integration for seasonal differencing.

- max.Q:

  The maximum order of the seasonal moving average (SMA) terms.

- ...:

  Additional arguments passed to
  [`forecast::auto.arima`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
