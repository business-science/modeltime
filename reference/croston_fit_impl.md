# Low-Level Exponential Smoothing function for translating modeltime to forecast

Low-Level Exponential Smoothing function for translating modeltime to
forecast

## Usage

``` r
croston_fit_impl(x, y, alpha = 0.1, ...)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- alpha:

  Value of alpha. Default value is 0.1.

- ...:

  Additional arguments passed to
  [`forecast::ets`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
