# General Interface for Boosted PROPHET Time Series Models

`prophet_boost()` is a way to generate a *specification* of a Boosted
PROPHET model before fitting and allows the model to be created using
different packages. Currently the only package is `prophet`.

## Usage

``` r
prophet_boost(
  mode = "regression",
  growth = NULL,
  changepoint_num = NULL,
  changepoint_range = NULL,
  seasonality_yearly = NULL,
  seasonality_weekly = NULL,
  seasonality_daily = NULL,
  season = NULL,
  prior_scale_changepoints = NULL,
  prior_scale_seasonality = NULL,
  prior_scale_holidays = NULL,
  logistic_cap = NULL,
  logistic_floor = NULL,
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

- growth:

  String 'linear' or 'logistic' to specify a linear or logistic trend.

- changepoint_num:

  Number of potential changepoints to include for modeling trend.

- changepoint_range:

  Adjusts the flexibility of the trend component by limiting to a
  percentage of data before the end of the time series. 0.80 means that
  a changepoint cannot exist after the first 80% of the data.

- seasonality_yearly:

  One of "auto", TRUE or FALSE. Toggles on/off a seasonal component that
  models year-over-year seasonality.

- seasonality_weekly:

  One of "auto", TRUE or FALSE. Toggles on/off a seasonal component that
  models week-over-week seasonality.

- seasonality_daily:

  One of "auto", TRUE or FALSE. Toggles on/off a seasonal componet that
  models day-over-day seasonality.

- season:

  'additive' (default) or 'multiplicative'.

- prior_scale_changepoints:

  Parameter modulating the flexibility of the automatic changepoint
  selection. Large values will allow many changepoints, small values
  will allow few changepoints.

- prior_scale_seasonality:

  Parameter modulating the strength of the seasonality model. Larger
  values allow the model to fit larger seasonal fluctuations, smaller
  values dampen the seasonality.

- prior_scale_holidays:

  Parameter modulating the strength of the holiday components model,
  unless overridden in the holidays input.

- logistic_cap:

  When growth is logistic, the upper-bound for "saturation".

- logistic_floor:

  When growth is logistic, the lower-bound for "saturation".

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
determine the *mode* of the model. For `prophet_boost()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "prophet_xgboost" (default) - Connects to
  [`prophet::prophet()`](https://rdrr.io/pkg/prophet/man/prophet.html)
  and
  [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

**Main Arguments**

The main arguments (tuning parameters) for the **PROPHET** model are:

- `growth`: String 'linear' or 'logistic' to specify a linear or
  logistic trend.

- `changepoint_num`: Number of potential changepoints to include for
  modeling trend.

- `changepoint_range`: Range changepoints that adjusts how close to the
  end the last changepoint can be located.

- `season`: 'additive' (default) or 'multiplicative'.

- `prior_scale_changepoints`: Parameter modulating the flexibility of
  the automatic changepoint selection. Large values will allow many
  changepoints, small values will allow few changepoints.

- `prior_scale_seasonality`: Parameter modulating the strength of the
  seasonality model. Larger values allow the model to fit larger
  seasonal fluctuations, smaller values dampen the seasonality.

- `prior_scale_holidays`: Parameter modulating the strength of the
  holiday components model, unless overridden in the holidays input.

- `logistic_cap`: When growth is logistic, the upper-bound for
  "saturation".

- `logistic_floor`: When growth is logistic, the lower-bound for
  "saturation".

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

Model 1: PROPHET:

|                          |                                |
|--------------------------|--------------------------------|
| modeltime                | prophet                        |
| growth                   | growth ('linear')              |
| changepoint_num          | n.changepoints (25)            |
| changepoint_range        | changepoints.range (0.8)       |
| seasonality_yearly       | yearly.seasonality ('auto')    |
| seasonality_weekly       | weekly.seasonality ('auto')    |
| seasonality_daily        | daily.seasonality ('auto')     |
| season                   | seasonality.mode ('additive')  |
| prior_scale_changepoints | changepoint.prior.scale (0.05) |
| prior_scale_seasonality  | seasonality.prior.scale (10)   |
| prior_scale_holidays     | holidays.prior.scale (10)      |
| logistic_cap             | df\$cap (NULL)                 |
| logistic_floor           | df\$floor (NULL)               |

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

**prophet_xgboost**

Model 1: PROPHET
([`prophet::prophet`](https://rdrr.io/pkg/prophet/man/prophet.html)):

    #> function (df = NULL, growth = "linear", changepoints = NULL, n.changepoints = 25,
    #>     changepoint.range = 0.8, yearly.seasonality = "auto", weekly.seasonality = "auto",
    #>     daily.seasonality = "auto", holidays = NULL, seasonality.mode = "additive",
    #>     seasonality.prior.scale = 10, holidays.prior.scale = 10, changepoint.prior.scale = 0.05,
    #>     mcmc.samples = 0, interval.width = 0.8, uncertainty.samples = 1000,
    #>     fit = TRUE, ...)

Parameter Notes:

- `df`: This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

- `holidays`: A data.frame of holidays can be supplied via
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

- `uncertainty.samples`: The default is set to 0 because the prophet
  uncertainty intervals are not used as part of the Modeltime Workflow.
  You can override this setting if you plan to use prophet's uncertainty
  tools.

Logistic Growth and Saturation Levels:

- For `growth = "logistic"`, simply add numeric values for
  `logistic_cap` and / or `logistic_floor`. There is *no need* to add
  additional columns for "cap" and "floor" to your data frame.

Limitations:

- [`prophet::add_seasonality()`](https://rdrr.io/pkg/prophet/man/add_seasonality.html)
  is not currently implemented. It's used to specify non-standard
  seasonalities using fourier series. An alternative is to use
  [`step_fourier()`](https://business-science.github.io/timetk/reference/step_fourier.html)
  and supply custom seasonalities as Extra Regressors.

Model 2: XGBoost
([`xgboost::xgb.train`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)):

    #> function (params = xgb.params(), data, nrounds, evals = list(), objective = NULL,
    #>     custom_metric = NULL, verbose = 1, print_every_n = 1L, early_stopping_rounds = NULL,
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

**Univariate (No Extra Regressors):**

For univariate analysis, you must include a date or date-time feature.
Simply use:

- Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.

- XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore
  xreg's.

**Multivariate (Extra Regressors)**

Extra Regressors parameter is populated using the
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
[`arima_reg()`](https://business-science.github.io/modeltime/reference/arima_reg.md)
using [`fit()`](https://generics.r-lib.org/reference/fit.html):

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
library(parsnip)
library(rsample)
library(timetk)

# Data
m750 <- m4_monthly %>% filter(id == "M750")
m750
#> # A tibble: 306 × 3
#>    id    date       value
#>    <fct> <date>     <dbl>
#>  1 M750  1990-01-01  6370
#>  2 M750  1990-02-01  6430
#>  3 M750  1990-03-01  6520
#>  4 M750  1990-04-01  6580
#>  5 M750  1990-05-01  6620
#>  6 M750  1990-06-01  6690
#>  7 M750  1990-07-01  6000
#>  8 M750  1990-08-01  5450
#>  9 M750  1990-09-01  6480
#> 10 M750  1990-10-01  6820
#> # ℹ 296 more rows

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# ---- PROPHET ----

# Model Spec
model_spec <- prophet_boost(
    learn_rate = 0.1
) %>%
    set_engine("prophet_xgboost")

# Fit Spec

model_fit <- model_spec %>%
    fit(log(value) ~ date + as.numeric(date) + month(date, label = TRUE),
        data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.
model_fit
#> parsnip model object
#> 
#> PROPHET w/ XGBoost Errors
#> ---
#> Model 1: PROPHET
#>  - growth: 'linear'
#>  - n.changepoints: 25
#>  - changepoint.range: 0.8
#>  - yearly.seasonality: 'auto'
#>  - weekly.seasonality: 'auto'
#>  - daily.seasonality: 'auto'
#>  - seasonality.mode: 'additive'
#>  - changepoint.prior.scale: 0.05
#>  - seasonality.prior.scale: 10
#>  - holidays.prior.scale: 10
#>  - logistic_cap: NULL
#>  - logistic_floor: NULL
#> 
#> ---
#> Model 2: XGBoost Errors
#> 
#> NULL
# }


```
