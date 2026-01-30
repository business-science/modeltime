# General Interface for Multiple Seasonality Regression Models (TBATS, STLM)

`seasonal_reg()` is a way to generate a *specification* of an Seasonal
Decomposition model before fitting and allows the model to be created
using different packages. Currently the only package is `forecast`.

## Usage

``` r
seasonal_reg(
  mode = "regression",
  seasonal_period_1 = NULL,
  seasonal_period_2 = NULL,
  seasonal_period_3 = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- seasonal_period_1:

  (required) The primary seasonal frequency. Uses `"auto"` by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided. See Fit Details
  below.

- seasonal_period_2:

  (optional) A second seasonal frequency. Is `NULL` by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided. See Fit Details
  below.

- seasonal_period_3:

  (optional) A third seasonal frequency. Is `NULL` by default. A
  character phrase of "auto" or time-based phrase of "2 weeks" can be
  used if a date or date-time variable is provided. See Fit Details
  below.

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `seasonal_reg()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "tbats" - Connects to
  [`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html)

- "stlm_ets" - Connects to
  [`forecast::stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.html),
  `method = "ets"`

- "stlm_arima" - Connects to
  [`forecast::stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.html),
  `method = "arima"`

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

|                                                         |                        |                        |
|---------------------------------------------------------|------------------------|------------------------|
| modeltime                                               | forecast::stlm         | forecast::tbats        |
| seasonal_period_1, seasonal_period_2, seasonal_period_3 | msts(seasonal.periods) | msts(seasonal.periods) |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

The engines use
[`forecast::stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.html).

Function Parameters:

    #> function (y, s.window = 7 + 4 * seq(6), robust = FALSE, method = c("ets",
    #>     "arima"), modelfunction = NULL, model = NULL, etsmodel = "ZZN", lambda = NULL,
    #>     biasadj = FALSE, xreg = NULL, allow.multiplicative.trend = FALSE, x = y,
    #>     ...)

**tbats**

- **Method:** Uses `method = "tbats"`, which by default is auto-TBATS.

- **Xregs:** Univariate. Cannot accept Exogenous Regressors (xregs).
  Xregs are ignored.

**stlm_ets**

- **Method:** Uses `method = "stlm_ets"`, which by default is auto-ETS.

- **Xregs:** Univariate. Cannot accept Exogenous Regressors (xregs).
  Xregs are ignored.

**stlm_arima**

- **Method:** Uses `method = "stlm_arima"`, which by default is
  auto-ARIMA.

- **Xregs:** Multivariate. Can accept Exogenous Regressors (xregs).

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

*Seasonal Period Specification*

The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`,
`seasonal_period = "12 months"`, or `seasonal_period = "yearly"`). There
are 3 ways to specify:

1.  `seasonal_period = "auto"`: A seasonal period is selected based on
    the periodicity of the data (e.g. 12 if monthly)

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

- The `tbats` engine *cannot* accept Xregs.

- The `stlm_ets` engine *cannot* accept Xregs.

- The `stlm_arima` engine *can* accept Xregs

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
`seasonal_reg()` using
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
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)

# Data
taylor_30_min
#> # A tibble: 4,032 × 2
#>    date                value
#>    <dttm>              <dbl>
#>  1 2000-06-05 00:00:00 22262
#>  2 2000-06-05 00:30:00 21756
#>  3 2000-06-05 01:00:00 22247
#>  4 2000-06-05 01:30:00 22759
#>  5 2000-06-05 02:00:00 22549
#>  6 2000-06-05 02:30:00 22313
#>  7 2000-06-05 03:00:00 22128
#>  8 2000-06-05 03:30:00 21860
#>  9 2000-06-05 04:00:00 21751
#> 10 2000-06-05 04:30:00 21336
#> # ℹ 4,022 more rows

# Split Data 80/20
splits <- initial_time_split(taylor_30_min, prop = 0.8)

# ---- STLM ETS ----

# Model Spec
model_spec <- seasonal_reg() %>%
    set_engine("stlm_ets")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 48 observations per 1 day
model_fit
#> parsnip model object
#> 
#> SEASONAL DECOMP: ETS(A,Ad,N)
#> 
#> # A tibble: 1 × 5
#>      aic    bic   aicc loglik       mse
#>    <dbl>  <dbl>  <dbl>  <dbl>     <dbl>
#> 1 -6473. -6437. -6473.  3243. 0.0000415


# ---- STLM ARIMA ----

# Model Spec
model_spec <- seasonal_reg() %>%
    set_engine("stlm_arima")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 48 observations per 1 day
model_fit
#> parsnip model object
#> 
#> SEASONAL DECOMP: ARIMA(3,1,2)
#> 
#> Series: x 
#> ARIMA(3,1,2) 
#> 
#> Coefficients:
#>          ar1     ar2      ar3      ma1      ma2
#>       1.0031  0.0782  -0.3096  -0.3203  -0.1378
#> s.e.  0.0838  0.1286   0.0575   0.0875   0.0817
#> 
#> sigma^2 = 3.918e-05:  log likelihood = 11786.32
#> AIC=-23560.65   AICc=-23560.62   BIC=-23524.18
```
