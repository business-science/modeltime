# General Interface for "Boosted" ARIMA Regression Models

`arima_boost()` is a way to generate a *specification* of a time series
model that uses boosting to improve modeling errors (residuals) on
Exogenous Regressors. It works with both "automated" ARIMA
(`auto.arima`) and standard ARIMA (`arima`). The main algorithms are:

- Auto ARIMA + XGBoost Errors (engine = `auto_arima_xgboost`, default)

- ARIMA + XGBoost Errors (engine = `arima_xgboost`)

## Usage

``` r
arima_boost(
  mode = "regression",
  seasonal_period = NULL,
  non_seasonal_ar = NULL,
  non_seasonal_differences = NULL,
  non_seasonal_ma = NULL,
  seasonal_ar = NULL,
  seasonal_differences = NULL,
  seasonal_ma = NULL,
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- seasonal_period:

  A seasonal frequency. Uses "auto" by default. A character phrase of
  "auto" or time-based phrase of "2 weeks" can be used if a date or
  date-time variable is provided. See Fit Details below.

- non_seasonal_ar:

  The order of the non-seasonal auto-regressive (AR) terms. Often
  denoted "p" in pdq-notation.

- non_seasonal_differences:

  The order of integration for non-seasonal differencing. Often denoted
  "d" in pdq-notation.

- non_seasonal_ma:

  The order of the non-seasonal moving average (MA) terms. Often denoted
  "q" in pdq-notation.

- seasonal_ar:

  The order of the seasonal auto-regressive (SAR) terms. Often denoted
  "P" in PDQ-notation.

- seasonal_differences:

  The order of integration for seasonal differencing. Often denoted "D"
  in PDQ-notation.

- seasonal_ma:

  The order of the seasonal moving average (SMA) terms. Often denoted
  "Q" in PDQ-notation.

- mtry:

  A number for the number (or proportion) of predictors that will be
  randomly sampled at each split when creating the tree models (specific
  engines only).

- trees:

  An integer for the number of trees contained in the ensemble.

- min_n:

  An integer for the minimum number of data points in a node that is
  required for the node to be split further.

- tree_depth:

  An integer for the maximum depth of the tree (i.e. number of splits)
  (specific engines only).

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- loss_reduction:

  A number for the reduction in the loss function required to split
  further (specific engines only).

- sample_size:

  number for the number (or proportion) of data that is exposed to the
  fitting routine.

- stop_iter:

  The number of iterations without improvement before stopping
  (`xgboost` only).

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `arima_boost()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "auto_arima_xgboost" (default) - Connects to
  [`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
  and
  [xgboost::xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

- "arima_xgboost" - Connects to
  [`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
  and
  [xgboost::xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

**Main Arguments**

The main arguments (tuning parameters) for the **ARIMA model** are:

- `seasonal_period`: The periodic nature of the seasonality. Uses "auto"
  by default.

- `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR)
  terms.

- `non_seasonal_differences`: The order of integration for non-seasonal
  differencing.

- `non_seasonal_ma`: The order of the non-seasonal moving average (MA)
  terms.

- `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.

- `seasonal_differences`: The order of integration for seasonal
  differencing.

- `seasonal_ma`: The order of the seasonal moving average (SMA) terms.

The main arguments (tuning parameters) for the model **XGBoost model**
are:

- `mtry`: The number of predictors that will be randomly sampled at each
  split when creating the tree models.

- `trees`: The number of trees contained in the ensemble.

- `min_n`: The minimum number of data points in a node that are required
  for the node to be split further.

- `tree_depth`: The maximum depth of the tree (i.e. number of splits).

- `learn_rate`: The rate at which the boosting algorithm adapts from
  iteration-to-iteration.

- `loss_reduction`: The reduction in the loss function required to split
  further.

- `sample_size`: The amount of data exposed to the fitting routine.

- `stop_iter`: The number of iterations without improvement before
  stopping.

These arguments are converted to their specific names at the time that
the model is fit.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
(See Engine Details below).

If parameters need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

Model 1: ARIMA:

|                                                            |                              |                                |
|------------------------------------------------------------|------------------------------|--------------------------------|
| modeltime                                                  | forecast::auto.arima         | forecast::Arima                |
| seasonal_period                                            | ts(frequency)                | ts(frequency)                  |
| non_seasonal_ar, non_seasonal_differences, non_seasonal_ma | max.p(5), max.d(2), max.q(5) | order = c(p(0), d(0), q(0))    |
| seasonal_ar, seasonal_differences, seasonal_ma             | max.P(2), max.D(1), max.Q(2) | seasonal = c(P(0), D(0), Q(0)) |

Model 2: XGBoost:

|                |                      |
|----------------|----------------------|
| modeltime      | xgboost::xgb.train   |
| tree_depth     | max_depth (6)        |
| trees          | nrounds (15)         |
| learn_rate     | eta (0.3)            |
| mtry           | colsample_bynode (1) |
| min_n          | min_child_weight (1) |
| loss_reduction | gamma (0)            |
| sample_size    | subsample (1)        |
| stop_iter      | early_stop           |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

**auto_arima_xgboost (default engine)**

Model 1: Auto ARIMA
([`forecast::auto.arima`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)):

    #> function (y, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2,
    #>     max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1,
    #>     start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = c("aicc", "aic",
    #>         "bic"), stepwise = TRUE, nmodels = 94, trace = FALSE, approximation = (length(x) >
    #>         150 | frequency(x) > 12), method = NULL, truncate = NULL, xreg = NULL,
    #>     test = c("kpss", "adf", "pp"), test.args = list(), seasonal.test = c("seas",
    #>         "ocsb", "hegy", "ch"), seasonal.test.args = list(), allowdrift = TRUE,
    #>     allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE,
    #>     num.cores = 2, x = y, ...)

Parameter Notes:

- All values of nonseasonal pdq and seasonal PDQ are maximums. The
  `auto.arima` will select a value using these as an upper limit.

- `xreg` - This should not be used since XGBoost will be doing the
  regression

Model 2: XGBoost
([`xgboost::xgb.train`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)):

    #> function (params = list(), data, nrounds, watchlist = list(), obj = NULL,
    #>     feval = NULL, verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
    #>     maximize = NULL, save_period = NULL, save_name = "xgboost.model", xgb_model = NULL,
    #>     callbacks = list(), ...)

Parameter Notes:

- XGBoost uses a `params = list()` to capture. Parsnip / Modeltime
  automatically sends any args provided as `...` inside of
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
  to the `params = list(...)`.

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

*Seasonal Period Specification*

The period can be non-seasonal (`seasonal_period = 1`) or seasonal (e.g.
`seasonal_period = 12` or `seasonal_period = "12 months"`). There are 3
ways to specify:

1.  `seasonal_period = "auto"`: A period is selected based on the
    periodicity of the data (e.g. 12 if monthly)

2.  `seasonal_period = 12`: A numeric frequency. For example, 12 is
    common for monthly data

3.  `seasonal_period = "1 year"`: A time-based phrase. For example, "1
    year" would convert to 12 for monthly data.

**Univariate (No xregs, Exogenous Regressors):**

For univariate analysis, you must include a date or date-time feature.
Simply use:

- Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.

- XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore
  xreg's.

**Multivariate (xregs, Exogenous Regressors)**

The `xreg` parameter is populated using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) function:

- Only `factor`, `ordered factor`, and `numeric` data will be used as
  xregs.

- Date and Date-time variables are not used as xregs

- `character` data should be converted to factor.

*Xreg Example:* Suppose you have 3 features:

1.  `y` (target)

2.  `date` (time stamp),

3.  `month.lbl` (labeled month as a ordered factor).

The `month.lbl` is an exogenous regressor that can be passed to the
`arima_boost()` using
[`fit()`](https://generics.r-lib.org/reference/fit.html):

- `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous
  regressor.

- `fit_xy(data[,c("date", "month.lbl")], y = data$y)` will pass x, where
  x is a data frame containing `month.lbl` and the `date` feature. Only
  `month.lbl` will be used as an exogenous regressor.

Note that date or date-time class values are excluded from `xreg`.

## See also

[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.html),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
# \donttest{
library(dplyr)
library(lubridate)
#> 
#> Attaching package: ‘lubridate’
#> The following object is masked from ‘package:greybox’:
#> 
#>     hm
#> The following objects are masked from ‘package:base’:
#> 
#>     date, intersect, setdiff, union
library(parsnip)
library(rsample)
library(timetk)

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# MODEL SPEC ----

# Set engine and boosting parameters
model_spec <- arima_boost(

    # ARIMA args
    seasonal_period = 12,
    non_seasonal_ar = 0,
    non_seasonal_differences = 1,
    non_seasonal_ma = 1,
    seasonal_ar     = 0,
    seasonal_differences = 1,
    seasonal_ma     = 1,

    # XGBoost Args
    tree_depth = 6,
    learn_rate = 0.1
) %>%
    set_engine(engine = "arima_xgboost")

# FIT ----

# Boosting - Happens by adding numeric date and month features
model_fit_boosted <- model_spec %>%
    fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
        data = training(splits))
model_fit_boosted
#> parsnip model object
#> 
#> ARIMA(0,1,1)(0,1,1)[12] w/ XGBoost Errors
#> ---
#> Model 1: Standard ARIMA
#> Series: outcome 
#> ARIMA(0,1,1)(0,1,1)[12] 
#> 
#> Coefficients:
#>           ma1     sma1
#>       -0.3405  -0.4781
#> s.e.   0.0652   0.0628
#> 
#> sigma^2 = 25114:  log likelihood = -1699.55
#> AIC=3405.1   AICc=3405.19   BIC=3415.8
#> 
#> ---
#> Model 2: XGBoost Errors
#> 
#> xgboost::xgb.train(params = list(eta = 0.1, max_depth = 6, gamma = 0, 
#>     colsample_bytree = 1, colsample_bynode = 1, min_child_weight = 1, 
#>     subsample = 1, objective = "reg:squarederror", nthread = 1), 
#>     data = x$data, nrounds = 15, evals = x$watchlist, verbose = 0)
# }

```
