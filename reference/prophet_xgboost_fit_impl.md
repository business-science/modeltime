# Low-Level PROPHET function for translating modeltime to Boosted PROPHET

Low-Level PROPHET function for translating modeltime to Boosted PROPHET

## Usage

``` r
prophet_xgboost_fit_impl(
  x,
  y,
  df = NULL,
  growth = "linear",
  changepoints = NULL,
  n.changepoints = 25,
  changepoint.range = 0.8,
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  holidays = NULL,
  seasonality.mode = "additive",
  seasonality.prior.scale = 10,
  holidays.prior.scale = 10,
  changepoint.prior.scale = 0.05,
  logistic_cap = NULL,
  logistic_floor = NULL,
  mcmc.samples = 0,
  interval.width = 0.8,
  uncertainty.samples = 1000,
  fit = TRUE,
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

- df:

  (optional) Dataframe containing the history. Must have columns ds
  (date type) and y, the time series. If growth is logistic, then df
  must also have a column cap that specifies the capacity at each ds. If
  not provided, then the model object will be instantiated but not fit;
  use fit.prophet(m, df) to fit the model.

- growth:

  String 'linear', 'logistic', or 'flat' to specify a linear, logistic
  or flat trend.

- changepoints:

  Vector of dates at which to include potential changepoints. If not
  specified, potential changepoints are selected automatically.

- n.changepoints:

  Number of potential changepoints to include. Not used if input
  \`changepoints\` is supplied. If \`changepoints\` is not supplied,
  then n.changepoints potential changepoints are selected uniformly from
  the first \`changepoint.range\` proportion of df\$ds.

- changepoint.range:

  Proportion of history in which trend changepoints will be estimated.
  Defaults to 0.8 for the first 80 \`changepoints\` is specified.

- yearly.seasonality:

  Fit yearly seasonality. Can be 'auto', TRUE, FALSE, or a number of
  Fourier terms to generate.

- weekly.seasonality:

  Fit weekly seasonality. Can be 'auto', TRUE, FALSE, or a number of
  Fourier terms to generate.

- daily.seasonality:

  Fit daily seasonality. Can be 'auto', TRUE, FALSE, or a number of
  Fourier terms to generate.

- holidays:

  data frame with columns holiday (character) and ds (date type)and
  optionally columns lower_window and upper_window which specify a range
  of days around the date to be included as holidays. lower_window=-2
  will include 2 days prior to the date as holidays. Also optionally can
  have a column prior_scale specifying the prior scale for each holiday.

- seasonality.mode:

  'additive' (default) or 'multiplicative'.

- seasonality.prior.scale:

  Parameter modulating the strength of the seasonality model. Larger
  values allow the model to fit larger seasonal fluctuations, smaller
  values dampen the seasonality. Can be specified for individual
  seasonalities using add_seasonality.

- holidays.prior.scale:

  Parameter modulating the strength of the holiday components model,
  unless overridden in the holidays input.

- changepoint.prior.scale:

  Parameter modulating the flexibility of the automatic changepoint
  selection. Large values will allow many changepoints, small values
  will allow few changepoints.

- logistic_cap:

  When growth is logistic, the upper-bound for "saturation".

- logistic_floor:

  When growth is logistic, the lower-bound for "saturation".

- mcmc.samples:

  Integer, if greater than 0, will do full Bayesian inference with the
  specified number of MCMC samples. If 0, will do MAP estimation.

- interval.width:

  Numeric, width of the uncertainty intervals provided for the forecast.
  If mcmc.samples=0, this will be only the uncertainty in the trend
  using the MAP estimate of the extrapolated generative model. If
  mcmc.samples\>0, this will be integrated over all model parameters,
  which will include uncertainty in seasonality.

- uncertainty.samples:

  Number of simulated draws used to estimate uncertainty intervals.
  Settings this value to 0 or False will disable uncertainty estimation
  and speed up the calculation.

- fit:

  Boolean, if FALSE the model is initialized but not fit.

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
