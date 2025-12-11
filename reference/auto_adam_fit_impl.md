# Low-Level ADAM function for translating modeltime to forecast

Low-Level ADAM function for translating modeltime to forecast

## Usage

``` r
auto_adam_fit_impl(
  x,
  y,
  period = "auto",
  p = 0,
  d = 0,
  q = 0,
  P = 0,
  D = 0,
  Q = 0,
  model = "ZXZ",
  constant = FALSE,
  regressors = c("use", "select", "adapt"),
  outliers = c("ignore", "use", "select"),
  level = 0.99,
  occurrence = c("none", "auto", "fixed", "general", "odds-ratio", "inverse-odds-ratio",
    "direct"),
  distribution = c("default", "dnorm", "dlaplace", "ds", "dgnorm", "dlnorm", "dinvgauss",
    "dgamma"),
  loss = c("likelihood", "MSE", "MAE", "HAM", "LASSO", "RIDGE", "MSEh", "TMSE", "GTMSE",
    "MSCE"),
  ic = c("AICc", "AIC", "BIC", "BICc"),
  select_order = FALSE,
  ...
)
```

## Arguments

- x:

  A data.frame of predictors

- y:

  A vector with outcome

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

- model:

  The type of ETS model.

- constant:

  Logical, determining, whether the constant is needed in the model or
  not.

- regressors:

  The variable defines what to do with the provided explanatory
  variables.

- outliers:

  Defines what to do with outliers.

- level:

  What confidence level to use for detection of outliers.

- occurrence:

  The type of model used in probability estimation.

- distribution:

  what density function to assume for the error term.

- loss:

  The type of Loss Function used in optimization.

- ic:

  The information criterion to use in the model selection / combination
  procedure.

- select_order:

  If TRUE, then the function will select the most appropriate order
  using a mechanism similar to auto.msarima(), but implemented in
  auto.adam(). The values list(ar=...,i=...,ma=...) specify the maximum
  orders to check in this case.

- ...:

  Additional arguments passed to
  [`smooth::auto.adam`](https://rdrr.io/pkg/smooth/man/adam.html)
