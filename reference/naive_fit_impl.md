# Low-Level NAIVE Forecast

Low-Level NAIVE Forecast

## Usage

``` r
naive_fit_impl(x, y, id = NULL, seasonal_period = "auto", ...)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- id:

  An optional ID feature to identify different time series. Should be a
  quoted name.

- seasonal_period:

  Not used for NAIVE forecast but here for consistency with SNAIVE

- ...:

  Not currently used
