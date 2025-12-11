# Low-Level NNETAR function for translating modeltime to forecast

Low-Level NNETAR function for translating modeltime to forecast

## Usage

``` r
nnetar_fit_impl(
  x,
  y,
  period = "auto",
  p = 1,
  P = 1,
  size = 10,
  repeats = 20,
  decay = 0,
  maxit = 100,
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

- p:

  Embedding dimension for non-seasonal time series. Number of
  non-seasonal lags used as inputs. For non-seasonal time series, the
  default is the optimal number of lags (according to the AIC) for a
  linear AR(p) model. For seasonal time series, the same method is used
  but applied to seasonally adjusted data (from an stl decomposition).
  If set to zero to indicate that no non-seasonal lags should be
  included, then P must be at least 1 and a model with only seasonal
  lags will be fit.

- P:

  Number of seasonal lags used as inputs.

- size:

  Number of nodes in the hidden layer. Default is half of the number of
  input nodes (including external regressors, if given) plus 1.

- repeats:

  Number of networks to fit with different random starting weights.
  These are then averaged when producing forecasts.

- decay:

  Parameter for weight decay. Default 0.

- maxit:

  Maximum number of iterations. Default 100.

- ...:

  Additional arguments passed to
  [`forecast::nnetar`](https://pkg.robjhyndman.com/forecast/reference/nnetar.html)
