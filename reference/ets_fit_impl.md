# Low-Level Exponential Smoothing function for translating modeltime to forecast

Low-Level Exponential Smoothing function for translating modeltime to
forecast

## Usage

``` r
ets_fit_impl(
  x,
  y,
  period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto",
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
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

- error:

  The form of the error term: "auto", "additive", or "multiplicative".
  If the error is multiplicative, the data must be non-negative.

- trend:

  The form of the trend term: "auto", "additive", "multiplicative" or
  "none".

- season:

  The form of the seasonal term: "auto", "additive", "multiplicative" or
  "none".

- damping:

  Apply damping to a trend: "auto", "damped", or "none".

- alpha:

  Value of alpha. If NULL, it is estimated.

- beta:

  Value of beta. If NULL, it is estimated.

- gamma:

  Value of gamma. If NULL, it is estimated.

- ...:

  Additional arguments passed to
  [`forecast::ets`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
