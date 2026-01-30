# Bridge ARIMA-XGBoost Modeling function

Bridge ARIMA-XGBoost Modeling function

## Usage

``` r
arima_xgboost_fit_impl(
  x,
  y,
  period = "auto",
  p = 0,
  d = 0,
  q = 0,
  P = 0,
  D = 0,
  Q = 0,
  include.mean = TRUE,
  include.drift = FALSE,
  include.constant = NULL,
  lambda = model$lambda,
  biasadj = FALSE,
  method = c("CSS-ML", "ML", "CSS"),
  model = NULL,
  max_depth = 6,
  nrounds = 15,
  eta = 0.3,
  colsample_bytree = NULL,
  colsample_bynode = NULL,
  min_child_weight = 1,
  gamma = 0,
  subsample = 1,
  validation = 0,
  early_stop = NULL,
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

  The order of the non-seasonal auto-regressive (AR) terms.

- d:

  The order of integration for non-seasonal differencing.

- q:

  The order of the non-seasonal moving average (MA) terms.

- P:

  The order of the seasonal auto-regressive (SAR) terms.

- D:

  The order of integration for seasonal differencing.

- Q:

  The order of the seasonal moving average (SMA) terms.

- include.mean:

  Should the ARIMA model include a mean term? The default is `TRUE` for
  undifferenced series, `FALSE` for differenced ones (where a mean would
  not affect the fit nor predictions).

- include.drift:

  Should the ARIMA model include a linear drift term? (i.e., a linear
  regression with ARIMA errors is fitted.) The default is `FALSE`.

- include.constant:

  If `TRUE`, then `include.mean` is set to be `TRUE` for undifferenced
  series and `include.drift` is set to be `TRUE` for differenced series.
  Note that if there is more than one difference taken, no constant is
  included regardless of the value of this argument. This is deliberate
  as otherwise quadratic and higher order polynomial trends would be
  induced.

- lambda:

  Box-Cox transformation parameter. If `lambda="auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

- biasadj:

  Use adjusted back-transformed mean for Box-Cox transformations. If
  transformed data is used to produce forecasts and fitted values, a
  regular back transformation will result in median forecasts. If
  biasadj is TRUE, an adjustment will be made to produce mean forecasts
  and fitted values.

- method:

  Fitting method: maximum likelihood or minimize conditional
  sum-of-squares. The default (unless there are missing values) is to
  use conditional-sum-of-squares to find starting values, then maximum
  likelihood.

- model:

  Output from a previous call to `Arima`. If model is passed, this same
  model is fitted to `y` without re-estimating any parameters.

- max_depth:

  An integer for the maximum depth of the tree.

- nrounds:

  An integer for the number of boosting iterations.

- eta:

  A numeric value between zero and one to control the learning rate.

- colsample_bytree:

  Subsampling proportion of columns.

- colsample_bynode:

  Subsampling proportion of columns for each node within each tree. See
  the `counts` argument below. The default uses all columns.

- min_child_weight:

  A numeric value for the minimum sum of instance weights needed in a
  child to continue to split.

- gamma:

  A number for the minimum loss reduction required to make a further
  partition on a leaf node of the tree

- subsample:

  Subsampling proportion of rows.

- validation:

  A positive number. If on `[0, 1)` the value, `validation` is a random
  proportion of data in `x` and `y` that are used for performance
  assessment and potential early stopping. If 1 or greater, it is the
  *number* of training set samples use for these purposes.

- early_stop:

  An integer or `NULL`. If not `NULL`, it is the number of training
  iterations without improvement before stopping. If `validation` is
  used, performance is base on the validation set; otherwise the
  training set is used.

- ...:

  Additional arguments passed to
  [`xgboost::xgb.train`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)
