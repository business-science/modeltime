# Low-Level Exponential Smoothing function for translating modeltime to forecast

Low-Level Exponential Smoothing function for translating modeltime to
forecast

## Usage

``` r
theta_fit_impl(x, y, ...)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- ...:

  Additional arguments passed to
  [`forecast::ets`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
