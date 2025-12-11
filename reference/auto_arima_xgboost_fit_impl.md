# Bridge ARIMA-XGBoost Modeling function

Bridge ARIMA-XGBoost Modeling function

## Usage

``` r
auto_arima_xgboost_fit_impl(
  x,
  y,
  period = "auto",
  max.p = 5,
  max.d = 2,
  max.q = 5,
  max.P = 2,
  max.D = 1,
  max.Q = 2,
  max.order = 5,
  d = NA,
  D = NA,
  start.p = 2,
  start.q = 2,
  start.P = 1,
  start.Q = 1,
  stationary = FALSE,
  seasonal = TRUE,
  ic = c("aicc", "aic", "bic"),
  stepwise = TRUE,
  nmodels = 94,
  trace = FALSE,
  approximation = (length(x) > 150 | frequency(x) > 12),
  method = NULL,
  truncate = NULL,
  test = c("kpss", "adf", "pp"),
  test.args = list(),
  seasonal.test = c("seas", "ocsb", "hegy", "ch"),
  seasonal.test.args = list(),
  allowdrift = TRUE,
  allowmean = TRUE,
  lambda = NULL,
  biasadj = FALSE,
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

- max.order:

  Maximum value of p+q+P+Q if model selection is not stepwise.

- d:

  Order of first-differencing. If missing, will choose a value based on
  `test`.

- D:

  Order of seasonal-differencing. If missing, will choose a value based
  on `season.test`.

- start.p:

  Starting value of p in stepwise procedure.

- start.q:

  Starting value of q in stepwise procedure.

- start.P:

  Starting value of P in stepwise procedure.

- start.Q:

  Starting value of Q in stepwise procedure.

- stationary:

  If `TRUE`, restricts search to stationary models.

- seasonal:

  If `FALSE`, restricts search to non-seasonal models.

- ic:

  Information criterion to be used in model selection.

- stepwise:

  If `TRUE`, will do stepwise selection (faster). Otherwise, it searches
  over all models. Non-stepwise selection can be very slow, especially
  for seasonal models.

- nmodels:

  Maximum number of models considered in the stepwise search.

- trace:

  If `TRUE`, the list of ARIMA models considered will be reported.

- approximation:

  If `TRUE`, estimation is via conditional sums of squares and the
  information criteria used for model selection are approximated. The
  final model is still computed using maximum likelihood estimation.
  Approximation should be used for long time series or a high seasonal
  period to avoid excessive computation times.

- method:

  fitting method: maximum likelihood or minimize conditional
  sum-of-squares. The default (unless there are missing values) is to
  use conditional-sum-of-squares to find starting values, then maximum
  likelihood. Can be abbreviated.

- truncate:

  An integer value indicating how many observations to use in model
  selection. The last `truncate` values of the series are used to select
  a model when `truncate` is not `NULL` and `approximation=TRUE`. All
  observations are used if either `truncate=NULL` or
  `approximation=FALSE`.

- test:

  Type of unit root test to use. See
  [`ndiffs`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.html)
  for details.

- test.args:

  Additional arguments to be passed to the unit root test.

- seasonal.test:

  This determines which method is used to select the number of seasonal
  differences. The default method is to use a measure of seasonal
  strength computed from an STL decomposition. Other possibilities
  involve seasonal unit root tests.

- seasonal.test.args:

  Additional arguments to be passed to the seasonal unit root test. See
  [`nsdiffs`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.html)
  for details.

- allowdrift:

  If `TRUE`, models with drift terms are considered.

- allowmean:

  If `TRUE`, models with a non-zero mean are considered.

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
