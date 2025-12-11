# Low-Level PROPHET function for translating modeltime to PROPHET

Low-Level PROPHET function for translating modeltime to PROPHET

## Usage

``` r
prophet_fit_impl(
  x,
  y,
  growth = "linear",
  n.changepoints = 25,
  changepoint.range = 0.8,
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  seasonality.mode = "additive",
  changepoint.prior.scale = 0.05,
  seasonality.prior.scale = 10,
  holidays.prior.scale = 10,
  regressors.prior.scale = 10000,
  regressors.standardize = "auto",
  regressors.mode = NULL,
  logistic_cap = NULL,
  logistic_floor = NULL,
  ...
)
```

## Arguments

- x:

  A dataframe of xreg (exogenous regressors)

- y:

  A numeric vector of values to fit

- growth:

  String 'linear', 'logistic', or 'flat' to specify a linear, logistic
  or flat trend.

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

- seasonality.mode:

  'additive' (default) or 'multiplicative'.

- changepoint.prior.scale:

  Parameter modulating the flexibility of the automatic changepoint
  selection. Large values will allow many changepoints, small values
  will allow few changepoints.

- seasonality.prior.scale:

  Parameter modulating the strength of the seasonality model. Larger
  values allow the model to fit larger seasonal fluctuations, smaller
  values dampen the seasonality. Can be specified for individual
  seasonalities using add_seasonality.

- holidays.prior.scale:

  Parameter modulating the strength of the holiday components model,
  unless overridden in the holidays input.

- regressors.prior.scale:

  Float scale for the normal prior. Default is 10,000. Gets passed to
  `prophet::add_regressor(prior.scale)`

- regressors.standardize:

  Bool, specify whether this regressor will be standardized prior to
  fitting. Can be 'auto' (standardize if not binary), True, or False.
  Gets passed to `prophet::add_regressor(standardize)`.

- regressors.mode:

  Optional, 'additive' or 'multiplicative'. Defaults to
  `seasonality.mode`.

- logistic_cap:

  When growth is logistic, the upper-bound for "saturation".

- logistic_floor:

  When growth is logistic, the lower-bound for "saturation".

- ...:

  Additional arguments passed to
  [`prophet::prophet`](https://rdrr.io/pkg/prophet/man/prophet.html)
