# Low-Level ARIMA function for translating modeltime to forecast

Low-Level ARIMA function for translating modeltime to forecast

## Usage

``` r
Arima_fit_impl(
  x,
  y,
  period = "auto",
  p = 0,
  d = 0,
  q = 0,
  P = 0,
  D = 0,
  Q = 0,
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

  The order of the non-seasonal auto-regressive (AR) terms. Often
  denoted "p" in pdq-notation.

- d:

  The order of integration for non-seasonal differencing. Often denoted
  "d" in pdq-notation.

- q:

  The order of the non-seasonal moving average (MA) terms. Often denoted
  "q" in pdq-notation.

- P:

  The order of the seasonal auto-regressive (SAR) terms. Often denoted
  "P" in PDQ-notation.

- D:

  The order of integration for seasonal differencing. Often denoted "D"
  in PDQ-notation.

- Q:

  The order of the seasonal moving average (SMA) terms. Often denoted
  "Q" in PDQ-notation.

- ...:

  Additional arguments passed to
  [`forecast::Arima`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
