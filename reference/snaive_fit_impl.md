# Low-Level SNAIVE Forecast

Low-Level SNAIVE Forecast

## Usage

``` r
snaive_fit_impl(x, y, id = NULL, seasonal_period = "auto", ...)
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

  The seasonal period to forecast into the future

- ...:

  Not currently used
